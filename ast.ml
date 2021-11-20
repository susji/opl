open Printf
module SS = Set.Make(String)

exception ASTError of string

let verbose = ref false
let indent_depth = ref 0
let indent_deeper () = indent_depth := !indent_depth + 1
let indent_shallower () = indent_depth := !indent_depth - 1
let sind () = sprintf "%.*s" (!indent_depth * 4) ""
let ind () = print_string (sind ())

let cur_block = ref 0

let aprint m =
  if !verbose then
    let _ = ind () in
    print_string (m ^ "\n")
  else
    ()

let set_verbosity v = verbose := v

(*
 * The following types describe our AST completely.
 *
 * XXX Use map or hash table the key-value pairs here.
 *
 * XXX Make an indenting wrapper to replace the redundant `sind ()` stuff.
 *
 * XXX Somewhat confusingly, we create a block for each BEGIN / END
 *     statement. AST-wise, this would permit scoping for each individual
 *     BEGIN/END section, but grammar-wise it's not permitted.
 *)
type block = {
  id: int;
  mutable stmts: stmt list;
  mutable vars: string list;
  mutable consts: (string * value) list;
  mutable parent: block_parent;
  mutable name: string;
  mutable procs: string_and_block list;
  mutable scope_vars: scope;
  mutable scope_consts: scope;
  mutable scope_procs : scope;
}
and string_and_block = string * block
and scope =
  | Void
  | Scope of string_and_block list
and block_parent =
  | Root
  | Block of block
and node =
  | NodeUnary of node * unary_op
  | NodeBinary of node * binary_op * node
  | NodeValue of value
  | NodeId of string
and binary_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eql
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
and unary_op =
  | Pos
  | Neg
  | Odd
and value =
  | Int of int
and decl =
  | DeclConst of string * value
  | DeclInt of string
and stmt =
  | Assign of string * node
  | Call of string
  | Begin of block
  | While of node * block
  | If of node * block
  | Decl of decl
  | Input of string
  | Output of node

let string_of_value = function
  | Int i -> string_of_int i

let int_of_value = function
  | Int i -> i
  | _ -> raise (ASTError "not an integer")

(*
 * Dumping AST structure.
 *)
let rec string_of_binary_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eql -> "="
  | Neq -> "#"
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="

and string_of_unary_op = function
  | Pos -> "+"
  | Neg -> "-"
  | Odd -> "ODD"

and string_of_node = function
  | NodeValue value ->
    string_of_value value
  | NodeBinary (left, op, right) ->
    sprintf "%s %s %s"
      (string_of_node left)
      (string_of_node right)
      (string_of_binary_op op)
  | NodeUnary (left, op) ->
    sprintf "%s %s" (string_of_unary_op op) (string_of_node left)
  | NodeId id ->
    sprintf "`%s`" id

and string_of_node' = function
  | NodeValue value ->
    string_of_value value
  | NodeBinary (_, op, _) ->
    sprintf "%s" (string_of_binary_op op)
  | NodeUnary (_, op) ->
    sprintf "%s" (string_of_unary_op op)
  | NodeId id ->
    sprintf "`%s`" id

and string_of_decl = function
  | DeclConst (name, value) ->
    sprintf "CONST: %s = %s" name (string_of_value value)
  | DeclInt (name) ->
    sprintf "INT: %s" name

and string_of_stmt = function
  | Assign (name, node) ->
    sprintf "%s%s := %s"
      (sind ())
      name
      (string_of_node node)
  | Call name ->
    sprintf "%sCALL %s" (sind ()) name
  | Begin block ->
    sprintf "%sBEGIN %s" (sind ()) (string_of_block block)
  | While (node, block) ->
    sprintf "%sWHILE (%s)%s" (sind ())
      (string_of_node node)
      (string_of_block block)
  | If (node, block) ->
    sprintf "%sIF (%s)%s" (sind ())
      (string_of_node node)
      (string_of_block block)
  | Decl decl ->
    sprintf "%sDECL: %s" (sind ()) (string_of_decl decl)
  | Input name ->
    sprintf "%s>>> %s" (sind ()) name
  | Output node ->
    sprintf "%s<<< %s" (sind ()) (string_of_node node)

and string_of_stmts block =
  sprintf "%s<Statements>\n%s" (sind ())
    (List.fold_left
       (fun acc s -> acc ^ (string_of_stmt s) ^ "\n")
       "" block.stmts)

and string_of_vars block =
  sprintf "%s<Variables>\n%s" (sind ())
    (List.fold_left
       (fun acc s -> acc ^ (sind ()) ^ s ^ "\n")
       "" block.vars)

and string_of_consts block =
  sprintf "%s<Constants>\n%s" (sind ())
    (List.fold_left
       (fun acc c ->
          let (name, value) = c in
          acc ^  (sind ()) ^ name ^ "=" ^ (string_of_value value) ^ "\n")
       "" block.consts)

and string_of_procs block =
  sprintf "%s<Procedures>\n%s" (sind ())
    (List.fold_left
       (fun acc c ->
          let (name, proc_block) = c in
          acc ^  (sind ()) ^ "PROCEDURE=" ^ name ^
          (string_of_block proc_block) ^ "\n")
       "" block.procs)

and string_of_scope scope =
  match scope with
  | Void -> sprintf "%sVOID\n" (sind ())
  | Scope l ->
    sprintf "%s<Scope>\n%s" (sind ())
      (List.fold_left
         (fun acc c ->
            let (name, _) = c in
            acc ^  (sind ()) ^ name ^ "\n")
         "" l)

and string_of_block block =
  indent_deeper ();
  let res =
    sprintf "\n%sBLOCK[%d]=%s\n%s%s%s%s%s%s" (sind ())
      block.id
      block.name
      (string_of_vars block)
      (string_of_consts block)
      (string_of_stmts block)
      (string_of_procs block)
      (string_of_scope block.scope_vars)
      (string_of_scope block.scope_consts) in
  indent_shallower ();
  res

let rec get_new_block parent name =
  cur_block := !cur_block + 1;
  {
    id = !cur_block;
    stmts = [];
    vars = [];
    consts = [];
    procs = [];
    scope_vars = Void;
    scope_consts = Void;
    scope_procs = Void;
    parent = parent;
    name = name;
  }

and get_root_block () = get_new_block Root "__main__"

let get_block_info block = block.name

let add_stmt stmt block =
  aprint (sprintf "{%s} Adding a statement\n" (get_block_info block));
  block.stmts <- block.stmts @ (stmt :: [])

and add_var name block =
  aprint (sprintf "{%s} Adding variable %s\n" (get_block_info block) name);
  block.vars <- block.vars @ (name::[])

and add_const name value block =
  aprint (sprintf "{%s} Adding constant %s=%s\n"
            (get_block_info block) name (string_of_value value));
  block.consts <- block.consts @ ((name, value)::[])

and add_proc name contents block =
  aprint (sprintf "{%s} Adding procedure %s\n" (get_block_info block) name);
  block.procs <- block.procs @ ((name, contents)::[])

let get_child_blocks block =
  let rec aux acc left =
    match left with
    | [] -> acc
    | s::ss -> (
        match s with
        | Begin block
        | While (_, block)
        | If (_, block) -> aux (block::acc) ss
        | _ -> aux acc ss)
  in aux [] block.stmts

and get_procs block =
  let rec aux acc left =
    match left with
    | [] -> acc
    | p::ps -> aux (p::acc) ps
  in aux [] block.procs

let add_to_scope scope name block =
  match scope with
  | Scope l -> Scope ((name, block)::l)
  | Void -> failwith "Uninitialized scope"

let list_of_scope scope =
  match scope with
  | Void -> []
  | Scope s ->
    let rec aux acc left =
      match left with
      | [] -> acc
      | (var, _)::vs -> aux (var::acc) vs
    in aux [] s

(*
 * Different statement handlers should be passed here. A string set of
 * interest will be returned. The solution here does unnecessary recursion
 * recursion as the forest is traversed more than once, but I suppose it
 * may simplify the presentation.
 *)
let rec get_stmts_set stmts getter =
  let rec aux acc rest =
    match rest with
    | [] -> acc
    | s::ss -> aux (SS.union acc (getter s)) ss
  in aux SS.empty stmts

and get_called_procs stmt =
  match stmt with
  | Call proc -> SS.singleton proc
  | _ -> SS.empty

and get_referenced_vars stmt =
  match stmt with
  | Assign (name', node) ->
    SS.union (SS.singleton name') (get_node_vars node)
  | While (node, block)
  | If (node, block) ->
    SS.union
      (get_node_vars node)
      (get_stmts_set block.stmts get_referenced_vars)
  | Begin (block) ->
    get_stmts_set block.stmts get_referenced_vars
  | Input name' -> SS.singleton name'
  | Output node -> get_node_vars node
  | _ -> SS.empty

and get_assignment stmt =
  match stmt with
  | Assign (name', node) -> Some name'
  | _ -> None

and get_node_vars node =
  match node with
  | NodeUnary (l, _) -> get_node_vars l
  | NodeBinary (l, _, r) -> SS.union (get_node_vars l) (get_node_vars r)
  | NodeId n -> SS.singleton n
  | _ -> SS.empty

let recurse_into_blocks block handler =
  List.iter (fun block' -> handler block') (get_child_blocks block);
  List.iter
    (fun (proc_name, proc_block) -> handler proc_block) (get_procs block)

let str_recurse_into_blocks block handler =
  let res_child = List.fold_left
      (fun acc block' -> acc ^ (handler block'))
      "" (get_child_blocks block) in
  let res_procs = List.fold_left
      (fun acc (proc_name, proc_block) -> acc ^ handler proc_block)
      "" (get_procs block) in
  res_child ^ res_procs

let strset_dump ss =
  let _ = SS.iter (fun e -> print_string (e ^ ", ")) ss
  in print_newline ()

let string_of_strset ss =
  SS.fold (fun e acc -> e ^ ", " ^ acc) ss ""

(* No set.of_list in 4.01.0 *)
let strset_of_list l =
  let rec aux acc rest =
    match rest with
    | [] -> acc
    | hd::tl -> aux (SS.add hd acc) tl
  in aux SS.empty l

let rec get_nested_block_vars block =
  let child_vars =
    List.fold_left (fun acc block -> acc @ get_nested_block_vars block)
      [] (get_child_blocks block) in
  child_vars @ block.vars