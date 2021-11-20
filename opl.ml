open Printf
open Parser
open Ast

let arg_tokens = ref false
let arg_ast = ref false
let arg_verbose = ref false
let arg_tree = ref false
let arg_compile = ref false

let dump_error_loc () =
  let (p_pos, p_column, kind) = Parser.get_pos () in
  printf "-- Was parsing %d:%d, latest token: %s --\n"
    p_pos p_column (Lexer.string_of_token kind)

let dump_block block =
  if !arg_ast then
    print_string (Ast.string_of_block block)

let erroneous_escape m =
  print_string m;
  dump_error_loc ();
  dump_block (Parser.get_root_block ());
  print_endline "\nExiting.";
  exit 2

let erroneous_tree_escape m =
  print_string m;
  print_endline "\nTree analysis error, exiting.";
  exit 3

let analyze block =
  print_endline "Doing tree analysis...";
  Scoping.resolve_scopes block None;
  Undeclared.resolve_missing_var_decls block;
  Undeclared.resolve_missing_proc_decls block;
  Consts.find_const_assignments block;
  Cse.eliminate_common_expr block;
  Folding.fold_constants block;
  print_endline "Treehugging completed."

let analyze_tree block =
  if !arg_tree then
    analyze block

let compile block =
  let fo = open_out "out.s" in
  if !arg_compile then
    begin
      List.iter
        (fun line -> fprintf fo "%s\n" line)
        (Codegen.generate block);
      close_out fo;
    end

let do_everything () =
  let lexbuf = Lexing.from_channel stdin in
  let toks = Token.gobble_tokens lexbuf in
  if !arg_tokens then
    Token.dump_tokens toks;
  Parser.set_verbosity !arg_verbose;
  Ast.set_verbosity !arg_verbose;
  try
    let root_block = Parser.parse toks in
    print_endline "Parsing succesful.";
    analyze_tree root_block;
    dump_block root_block;
    compile root_block
  with
  | Parser.SyntaxError' e ->
    erroneous_escape (sprintf "%s\n" e)
  | Parser.SyntaxError (e, t, l, c) ->
    erroneous_escape
      (sprintf "[%d:%d:{%s}] %s\n" l c (Lexer.string_of_token t) e)
  | Scoping.ScopeError e ->
    erroneous_tree_escape e
  | Undeclared.UndeclaredError e ->
    erroneous_tree_escape e
  | Consts.ConstError e ->
    erroneous_tree_escape e
  | Cse.CSEError e ->
    erroneous_tree_escape e
  | Match_failure (file, line, col) ->
    erroneous_escape
      (sprintf "Match failed @%s:%d:%d\n" file line col)

let verify_args () =
  if !arg_compile && not !arg_tree then
    failwith "Code emission requires -w."

let do_args () =
  let args =
    [("-t", Arg.Set arg_tokens, "Dump tokens");
     ("-v", Arg.Set arg_verbose, "Verbose parsing");
     ("-a", Arg.Set arg_ast, "Dump the abstract syntax tree");
     ("-c", Arg.Set arg_compile, "Generate assembly");
     ("-w", Arg.Set arg_tree, "AST analysis and priming")] in
  let usage = "opl!" in
  Arg.parse args print_endline usage

let _ =
  do_args ();
  verify_args ();
  do_everything ()
