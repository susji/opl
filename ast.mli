module SS :
sig
  type elt = String.t
  type t = Set.Make(String).t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
end
exception ASTError of string
val set_verbosity : bool -> unit
type block = {
  id : int;
  mutable stmts : stmt list;
  mutable vars : string list;
  mutable consts : (string * value) list;
  mutable parent : block_parent;
  mutable name : string;
  mutable procs : string_and_block list;
  mutable scope_vars : scope;
  mutable scope_consts : scope;
  mutable scope_procs : scope;
}
and string_and_block = string * block
and scope = Void | Scope of string_and_block list
and block_parent = Root | Block of block
and node =
    NodeUnary of node * unary_op
  | NodeBinary of node * binary_op * node
  | NodeValue of value
  | NodeId of string
and binary_op = Add | Sub | Mul | Div | Eql | Neq | Lt | Gt | Leq | Geq
and unary_op = Pos | Neg | Odd
and value = Int of int
and decl = DeclConst of string * value | DeclInt of string
and stmt =
    Assign of string * node
  | Call of string
  | Begin of block
  | While of node * block
  | If of node * block
  | Decl of decl
  | Input of string
  | Output of node
val string_of_block : block -> string
val get_new_block : block_parent -> string -> block
val get_root_block : unit -> block
val get_block_info : block -> string
val add_stmt : stmt -> block -> unit
val add_var : string -> block -> unit
val add_const : string -> value -> block -> unit
val add_proc : string -> block -> block -> unit
val get_child_blocks : block -> block list
val get_procs : block -> string_and_block list
val add_to_scope : scope -> string -> block -> scope
val list_of_scope : scope -> string list
val get_stmts_set : stmt list -> (stmt -> SS.t) -> SS.t
val get_called_procs : stmt -> SS.t
val get_referenced_vars : stmt -> SS.t
val get_node_vars : node -> SS.t
val get_assignment : stmt -> string option
val get_nested_block_vars : block -> string list
val recurse_into_blocks : block -> (block -> unit) -> unit
val str_recurse_into_blocks : block -> (block -> string) -> string
val strset_dump : SS.t -> unit
val string_of_strset : SS.t -> string
val strset_of_list : SS.elt list -> SS.t
val int_of_value : value -> int
val string_of_node : node -> string
val string_of_node' : node -> string
val string_of_binary_op : binary_op -> string