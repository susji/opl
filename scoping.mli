exception ScopeError of string
val resolve_scopes : Ast.block -> Ast.block option -> unit
