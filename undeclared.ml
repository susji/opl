open Ast

exception UndeclaredError of string

let tnote m = Printf.printf "[missing] %s\n" m

(*
 * As we have prebuilt the variable and constant scopes for all our blocks,
 * detecting usage of undeclareds becomes a matter of doing the following
 * for each block:
 *
 * { undeclared_refs } = { node_refs } - ({ node_refs } INTERSECT { scope }),
 *
 * where { node_refs } is the set of variables/consts found in block's
 * statements containing nodes.
 *)

let get_scope_missing scope refs err =
  let scopes_refs_share = SS.inter scope refs in
  let not_in_scope = SS.filter (fun e -> not (SS.mem e scope)) refs in
  tnote "Scope contents:";
  strset_dump scope;
  tnote "Found references";
  strset_dump scopes_refs_share;
  tnote "Missing from scope:";
  strset_dump not_in_scope;
  if not (SS.is_empty not_in_scope) then
    raise (UndeclaredError
             (err ^ (string_of_strset not_in_scope)))

let rec resolve_missing_var_decls block =
  let scope_vars = strset_of_list (list_of_scope block.scope_vars) in
  let scope_consts = strset_of_list (list_of_scope block.scope_consts) in
  let scope = SS.union scope_vars scope_consts in
  let refd_vars = get_stmts_set block.stmts get_referenced_vars in
  get_scope_missing scope refd_vars "Undeclared identifier(s): ";
  recurse_into_blocks block resolve_missing_var_decls

let rec resolve_missing_proc_decls block =
  let scope_procs = strset_of_list (list_of_scope block.scope_procs) in
  let refd_procs = get_stmts_set block.stmts get_called_procs in
  get_scope_missing scope_procs refd_procs "Undeclared procedure(s): ";
  recurse_into_blocks block resolve_missing_proc_decls
