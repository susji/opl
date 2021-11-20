(* The following is a LL(1) parser. The precedence levels, namely
 * for separating + and - from * and /, are done the classic way
 * following Aho et al. (EBNF from Wikipedia.)
 *
 *  XXX Do the following the make this more readable:
 *      - Gather all tokens to a list
 *      - Use list matching to 'expect' less verbosely
*)
open Printf
open Lexing
open Lexer

exception SyntaxError' of string
exception SyntaxError of string * token' * int * int

let verbose = ref false
let toks = ref []
let cur_line = ref 0
let cur_column = ref 0
let root_block = ref (Ast.get_root_block ())

let get_root_block () =
  !root_block

let indent_depth = ref 0
let indent_deeper () = indent_depth := !indent_depth + 1
let indent_shallower () = indent_depth := !indent_depth - 1

let ind () = printf "%.*s" (!indent_depth * 2) ""; ()

let parnote m =
  if not !verbose then
    ()
  else
    let _= ind () in
    printf "[%d:%d] %s\n" !cur_line !cur_column m

let curtok_o (): token =
  match !toks with
  | [] -> raise (SyntaxError' "Ran out of tokens")
  | t::_ -> t

let curtok (): token' =
  let kind = (curtok_o ())#kind in
  parnote ("Curtok'd " ^ (string_of_token kind));
  kind

let get_pos () =
  (!cur_line, !cur_column, curtok ())

let update_pos () =
  let tok = curtok_o () in
  cur_line := tok#line;
  cur_column := tok#column

let consume () =
  match !toks with
  | [] -> raise (SyntaxError' "Nothing to consume")
  | t::ts ->
    parnote ("Consumed " ^ (string_of_token t#kind));
    toks := ts;
    update_pos ()

let nexttok (): token' =
  let tok = curtok_o () in
  parnote ("Nexttok'd " ^ (string_of_token tok#kind));
  consume ();
  tok#kind

let tokerr m =
  raise (SyntaxError (m, (curtok_o ())#kind, !cur_line, !cur_column))

let accept t =
  let cur = curtok_o () in
  if cur#kind = t then
    consume ()
  else
    tokerr
      (sprintf "Expecting %s, but found %s"
         (string_of_token t) (string_of_token cur#kind))

let rec const cb =
  parnote "const";
  match nexttok () with
  | ID name -> (
      accept EQL;
      let t = nexttok () in
      match t with
      | INT value -> (
          Ast.add_const name (Ast.Int value) cb;
          match nexttok () with
          | COMMA -> const cb
          | SEMICOLON -> ()
          | _ -> tokerr "Expecting ',' or ';'"
        )
      | _ -> tokerr "Expecting const value"
    )
  | _ -> tokerr "Expecting const identifier"

and var cb =
  parnote "var";
  match nexttok () with
  | ID name -> (
      let _ = Ast.add_var name cb in
      match nexttok () with
      | COMMA ->
        var cb
      | SEMICOLON -> ()
      | _ -> tokerr "Expecting ',' or ';'"
    )
  | _ -> tokerr "Expecting variable identifier"

and call cb =
  parnote "call";
  match nexttok () with
  | ID id -> Ast.add_stmt (Ast.Call id) cb
  | _ -> tokerr "Expecting call identifier"

and quest cb =
  parnote "quest";
  match nexttok () with
  | ID id -> Ast.add_stmt (Ast.Input id) cb
  | _ -> tokerr "Expecting input identifier"

and exclam cb =
  parnote "exclam";
  let node = expression () in
  Ast.add_stmt (Ast.Output node) cb

and beg cb =
  parnote ("begin in " ^ Ast.get_block_info cb);
  indent_deeper ();
  let nb = Ast.get_new_block (Ast.Block cb) "begin" in
  let rec gobble_stmts () =
    let _ = stmt nb in
    match curtok () with
    | SEMICOLON ->
      accept SEMICOLON;
      gobble_stmts ()
    | END ->
      accept END;
      Ast.add_stmt (Ast.Begin nb) cb;
      indent_shallower ()
    | _ -> tokerr "Expecting END to match BEGIN or ';'"
  in gobble_stmts ()

and cond_op () =
  parnote "cond_op";
  let kind = nexttok () in
  match kind with
  | EQL -> Ast.Eql
  | LT -> Ast.Lt
  | GT -> Ast.Gt
  | LEQ -> Ast.Leq
  | GEQ -> Ast.Geq
  | HASH -> Ast.Neq
  | _ -> tokerr "Expecting a conditional operator"

and condition cb =
  parnote "condition";
  match curtok () with
  | ODD ->
    accept ODD;
    let node = expression () in
    Ast.NodeUnary (node, Ast.Odd)
  | _ ->
    let lhs = expression () in
    let op = cond_op () in
    let rhs = expression () in
    Ast.NodeBinary (lhs, op, rhs)

and if_ cb =
  parnote "if_";
  let nb = Ast.get_new_block (Ast.Block cb) "if" in
  let nc = condition cb in
  let _ = accept THEN in
  let _ = stmt nb in
  Ast.add_stmt (Ast.If (nc, nb)) cb

and while_ cb =
  parnote "while_";
  let nb = Ast.get_new_block (Ast.Block cb) "while" in
  let nc = condition cb in
  let _ = accept DO in
  let _ = stmt nb in
  Ast.add_stmt (Ast.While (nc, nb)) cb

and assignment id cb =
  parnote "assignment";
  accept IS;
  Ast.add_stmt (Ast.Assign (id, expression ())) cb

and stmt cb =
  parnote "stmt";
  match nexttok () with
  | CALL -> call cb
  | QUEST -> quest cb
  | EXCLAM -> exclam cb
  | BEGIN -> beg cb
  | IF -> if_ cb
  | WHILE -> while_ cb
  | ID id -> assignment id cb
  | _ -> tokerr "Unrecognized statement"

and procedure cb =
  parnote "procedure";
  match nexttok () with
  | ID id -> (
      let nb = Ast.get_new_block (Ast.Block cb) ("procedure: " ^ id) in
      parnote ("Procedure {" ^ id ^ "} starting");
      accept SEMICOLON;
      block nb;
      accept SEMICOLON;
      parnote ("Procedure {" ^ id ^ "} parsed");
      Ast.add_proc id nb cb;
      match curtok () with
      | PROCEDURE ->
        accept PROCEDURE;
        procedure cb
      | _ -> ())
  | _ -> tokerr "Expecting procedure identifier"

and factor (): Ast.node =
  parnote "factor";
  match curtok () with
  | ID id ->
    consume ();
    Ast.NodeId id
  | INT value ->
    consume ();
    Ast.NodeValue (Ast.Int value)
  | LPAREN ->
    let _ = accept LPAREN in
    let node = expression () in
    let _ = accept RPAREN in
    node
  | _ -> tokerr "Expecting identifier, integer number, or '('"

and term (): Ast.node =
  parnote "term";
  let rec gobble_factors () =
    let lhs = factor () in
    match curtok () with
    | ASTERISK ->
      accept ASTERISK;
      Ast.NodeBinary (lhs, Ast.Mul, gobble_factors ())
    | FSLASH ->
      accept FSLASH;
      Ast.NodeBinary (lhs, Ast.Div, gobble_factors ())
    | _ -> lhs
  in gobble_factors ()

and expression (): Ast.node =
  parnote "expression";
  let rec gobble_terms () =
    let lhs = term () in
    match curtok () with
    | PLUS ->
      accept PLUS;
      Ast.NodeBinary (lhs, Ast.Add, gobble_terms ())
    | MINUS ->
      accept MINUS;
      Ast.NodeBinary (lhs, Ast.Sub, gobble_terms ())
    | _ -> lhs
  in match curtok () with
  | PLUS ->
    accept PLUS;
    Ast.NodeUnary (term (), Ast.Pos)
  | MINUS ->
    accept MINUS;
    Ast.NodeUnary (term (), Ast.Neg)
  | _ -> gobble_terms ()

and block cb =
  parnote "block";
  (match curtok () with
   | CONST ->
     accept CONST;
     const cb
   | _ -> ());
  (match curtok () with
   | VAR ->
     accept VAR;
     var cb
   | _ -> ());
  (match curtok () with
   | PROCEDURE ->
     accept PROCEDURE;
     indent_deeper ();
     procedure cb;
     indent_shallower ()
   | _ -> ());
  stmt cb

and program () =
  parnote "program";
  root_block := Ast.get_root_block();
  let _ = block !root_block in
  match curtok () with
  | PERIOD ->
    parnote "Parsing succesful.";
    !root_block
  | _ -> tokerr "Expecting '.' to terminate program."

and parse t =
  parnote "parse";
  toks := t;
  update_pos ();
  program ()

and set_verbosity v =
  verbose := v