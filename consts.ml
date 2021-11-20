open Ast

exception ConstError of string

let tnote m = Printf.printf "[const] %s\n" m

let get_assigned_consts block =
  let rec loop_stmts acc left =
    match left with
    | [] -> acc
    | stmt::tail ->
      let ass = get_assignment stmt in
      begin
        match ass with
        | Some name -> loop_stmts (SS.add name acc) tail
        | None -> loop_stmts acc tail
      end
  in loop_stmts SS.empty block.stmts

let no_scope_assignments scope_consts assignments =
  let consts_set = strset_of_list scope_consts in
  let assigned_consts =
    SS.filter (fun e -> SS.mem e consts_set) assignments in
  SS.iter (fun e -> tnote ("Assigning to const " ^ e)) assigned_consts;
  SS.is_empty assigned_consts

(*
 * We rely on scope resolving performed before checking for const
 * violation. After that, it becomes an exercise of recursively
 * checking all blocks whether their assignments are directed
 * in a constant.
 *)
let find_const_assignments block =
  let ec = ref 0 in
  let rec aux cur_block =
    let consts = list_of_scope cur_block.scope_consts in
    let assigns_to = get_assigned_consts cur_block in
    List.iter (fun e -> tnote ("found const " ^ e)) consts;
    SS.iter (fun e -> tnote ("assign -> " ^ e)) assigns_to;
    if not (no_scope_assignments consts assigns_to) then
      ec := !ec + 1;
    Ast.recurse_into_blocks cur_block aux
  in aux block;
  if !ec > 0 then
    raise (ConstError ("Constant assigments found: " ^ (string_of_int !ec)))