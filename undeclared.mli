exception UndeclaredError of string
val resolve_missing_var_decls : Ast.block -> unit
val resolve_missing_proc_decls: Ast.block -> unit