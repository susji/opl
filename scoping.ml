open Ast

let tnote m = Printf.printf "[scoping] %s\n" m

exception ScopeError of string

(*
 * Recursively build variable scopes for all blocks from the root inwards.
 * We forbid consts shadowing vars and vice versa. Procedure redefinitions
 * are not permitted.
 *)
let rec add_var block name =
  let report () =
    if var_in_scope block name then
      tnote ("variable " ^ name ^ " shadowed")
    else
      tnote ("variable " ^ name ^ " declared")
  in report ();
  if const_in_scope block name then
    raise (ScopeError ("Variable ``" ^ name ^ "'' trying to shadow constant"))
  else
    block.scope_vars <- Ast.add_to_scope (block.scope_vars) name block

and add_const block const =
  let (const_name, const_value) = const in
  let report () =
    if const_in_scope block const_name then
      tnote ("constant " ^ const_name ^ " shadowed")
    else
      tnote ("constant " ^ const_name ^ " declared")
  in report ();
  if var_in_scope block const_name then
    raise (ScopeError
             ("Constant ``" ^ const_name ^ "'' trying to shadow variable"))
  else
    block.scope_consts <-
      Ast.add_to_scope (block.scope_consts) const_name block

and add_proc block (proc: Ast.string_and_block) =
  let (proc_name, proc_block) = proc in
  if proc_in_scope block proc_name then
    raise (ScopeError ("Procedure ``" ^ proc_name ^ "'' already declared"))
  else
    block.scope_procs <-
      Ast.add_to_scope (block.scope_procs) proc_name block

and var_in_scope block name =
  match block.scope_vars with
  | Ast.Scope l -> (
      try
        ignore (List.assoc name l);
        true
      with
      | Not_found -> false)
  | Ast.Void -> failwith "Uninitialized variable scope"

and const_in_scope block name =
  match block.scope_consts with
  | Ast.Scope l -> (
      try
        ignore (List.assoc name l);
        true
      with
      | Not_found -> false)
  | Ast.Void -> failwith "Uninitialized variable scope"

and proc_in_scope block name =
  match block.scope_procs with
  | Ast.Scope l -> (
      try
        ignore (List.assoc name l);
        true
      with
      | Not_found -> false)
  | Ast.Void -> failwith "Uninitialized procedure scope"

let inherit_scope_vars parent =
  match parent with
  | None -> Ast.Scope []
  | Some parent' -> parent'.scope_vars

let inherit_scope_consts parent =
  match parent with
  | None -> Ast.Scope []
  | Some parent' -> parent'.scope_consts

let inherit_scope_procs parent =
  match parent with
  | None -> Ast.Scope []
  | Some parent' -> parent'.scope_procs

let resolve_scopes block parent =
  let _ = tnote "Resolving variable and constant scopes..." in
  let rec resolve block parent =
    block.scope_vars <- inherit_scope_vars parent;
    block.scope_consts <- inherit_scope_consts parent;
    block.scope_procs <- inherit_scope_procs parent;
    List.iter (fun v -> add_var block v) block.vars;
    List.iter (fun c -> add_const block c) block.consts;
    List.iter (fun p -> add_proc block p) block.procs;
    List.iter
      (fun block' -> resolve block' (Some block))
      (Ast.get_child_blocks block);
    List.iter
      (fun (proc_name, proc_block) ->
         tnote ("Resolving scope for procedure " ^ proc_name);
         resolve proc_block (Some block))
      (Ast.get_procs block)
  in resolve block parent