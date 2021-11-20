(*
 * The parsing API should only reveal the singular parse
 * function, which produces the AST root block. The root
 * block should then be used to tree-walk in the next phase.
 *)
val set_verbosity: bool -> unit
val parse: Lexer.token list -> Ast.block
val get_pos: unit -> int * int * Lexer.token'
val get_root_block: unit -> Ast.block

exception SyntaxError' of string
exception SyntaxError of string * Lexer.token' * int * int
