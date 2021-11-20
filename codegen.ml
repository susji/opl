(*
 * Generates AT & T assembly suitable for the GNU Assembler. No register
 * allocation is even attempted, and we stick with a stack-machine style.
 *
 * We attempt to obey the System V ABI where it makes sense. We assume
 * that all variables are integers and of 8 bytes in size. Since we
 * barely use any registers, we don't need to worry about clobbering.
 * We'll always assume signed integers.
 *
 * Our function calls are not used within expressions, so we don't need to
 * worry about function calls spoiling our register contents.
 *
 * XXX Instead of just sprinting whatever raw instructions, we should have
 *     have an architecture-independent code generator, which uses the
 *     emission commands from the architecture definition. This is the
 *     quick-and-dirty version.
 *)
open Ast
open Printf

exception CodegenNotFound
exception CodegenError of string

let rega = "%rax"
let regal = "%al"
let regb = "%r11"
let regcomp = "%cl"
let regcompq = "%rcx"
let pv_format = "_t_val_print"

let jmp_counter = ref 0
let iter_counter = ref 0

let output = ref []

let get_global_ref n = sprintf "_global_%s" n

let var_names_done = Hashtbl.create 100

let get_var_name id = sprintf "_var_%s" id

let add_var_name id =
  try
    let _ = Hashtbl.find var_names_done id in ""
  with Not_found ->
    Hashtbl.add var_names_done id 1;
    get_var_name id

(* Emit a line of output. *)
let e s = output := s::(!output)
let comment c = e (sprintf "# %s" c)

(*
 * All non-global variables are supposed to be stored in procedure
 * call frames. Thus when searching for a variable, we'll search the
 * block and its parents until we reach the Root block. If it's not found
 * until Root, then it must be a global. At this stage we should be confident
 * that the variable has actually been declared, since we survived the
 * semantic analysis.
 *)
let get_stack_ref block var =
  let rec aux block acc left =
    if block.parent = Root then
      raise CodegenNotFound
    else
      match left with
      | [] ->
        begin
          match block.parent with
          | Block parent -> aux parent acc parent.vars
          | _ -> failwith "NOTREACHED"
        end
      | e::tail ->
        if e = var then
          sprintf "-%d(%%rbp)" (acc * 8)
        else
          aux block (acc + 1) tail
  in aux block 0 block.vars

let is_id_local block id =
  try let _ = get_stack_ref block id in true
  with CodegenNotFound -> false

(*
 * We have already done scope-building before, so we may just recurse and
 * search until we find the constant value - or don't find.
 *)
let get_const_value block id =
  let rec aux block left =
    match left with
    | [] ->
      if block.parent = Root then
        raise CodegenNotFound
      else
        begin
          match block.parent with
          | Block parent -> aux parent parent.consts
          | _ -> failwith "NOTREACHED"
        end
    | (const, value)::tail ->
      if const = id then
        "$" ^ string_of_int (int_of_value value)
      else
        aux block tail
  in aux block block.consts

let is_id_const block id =
  try let _ = get_const_value block id in true
  with CodegenNotFound -> false

(*
 * At this stage we are confident that the scopes are built and checked
 * for name collisions such that variables may not shadow consts and
 * vice versa. Thus we can just check the existence of these in arbitrary
 * order.
 *
 * XXX Yes, we duplicate the effort by first doing the try/with exercise,
 *     and then researching again. Refactor this.
 *)
let get_var_ref block id =
  if is_id_local block id then get_stack_ref block id
  else if is_id_const block id then get_const_value block id
  else get_global_ref id

let emit_header m =
  comment m;
  e ".globl main"

and emit_proc_decls block =
  List.iter
    (fun (proc, _) -> e (sprintf ".globl %s" proc))
    block.procs

(* Seems like a fairly dumb way to generate id strings. *)
let emit_common block =
  let dump_id_names block =
    List.iter
      (fun id -> e (sprintf "\t%s: .asciz \"%s\"" (get_var_name id) id))
      block.vars;
    List.iter
      (fun (id, _) ->
         e (sprintf "\t%s: .asciz \"%s\"" (get_var_name id) id))
      block.consts
  in
  e "\t_greetings: .asciz \"Hello, PL/0!\"";
  e (sprintf "\t%s: .asciz \"%%s := %%lld\\n\"" pv_format);
  dump_id_names block;
  recurse_into_blocks block dump_id_names

(*
 * All variables belong to .bss. If we were to support initializers, then
 * .data would be suitable for variables.
 *)
let emit_global_allocs block =
  List.iter
    (fun name -> e (sprintf "\t.lcomm %s,8" (get_global_ref name)))
    block.vars

(*
 * We'll be (de)allocating stack for the whole procedure call, including
 * whatever is defined in the nested blocks. This permits us to be less
 * strict with the scoping rules if need be. Unless I remember wrong,
 * System V ABI requires stack alignment of 16 for function calls, so
 * we'll round up for each frame.
 *
 * XXX Enforce that this is called on a non-nested procedure block.
 *)
let stack_size_of_block block =
  let size = List.length (get_nested_block_vars block) * 8 in
  size + (size mod 16)

let emit_proc_entry ?(do_stack=true) proc pblock =
  let ssize = stack_size_of_block pblock in
  comment ("<< " ^ proc ^ " >>");
  e (sprintf "%s:" proc);
  if ssize > 0 && do_stack then
    begin
      e "push %rbp";
      e "mov %rsp, %rbp";
      e (sprintf "sub $%d, %%rsp" ssize);
    end

let emit_proc_depart ?(do_stack=true) proc pblock =
  let ssize = stack_size_of_block pblock in
  if ssize > 0 && do_stack then
    begin
      e (sprintf "add $%d, %%rsp" ssize);
      e "pop %rbp";
    end;
  e "ret"

(*
 * Statements and expressions.
 *)
let rec emit_stmts block =
  List.iter
    (fun stmt -> emit_stmt block stmt)
    block.stmts

and emit_stmt block stmt =
  match stmt with
  | Assign (var, node) -> emit_assign block var node
  | Call func -> e (sprintf "call %s" func)
  | Begin block' -> emit_stmts block'
  | While (node, block') -> emit_while block' node
  | If (node, block') -> emit_if block' node
  | Decl _ -> ()
  | Input var -> comment ("input: " ^ var)
  | Output node -> comment "output"

and emit_if block node =
  let () = jmp_counter := !jmp_counter + 1 in
  let label_after = sprintf "_if_after_%d" !jmp_counter in
  emit_expr block node;
  e (sprintf "pop %s" rega);
  e (sprintf "cmp $0, %s" rega);
  e (sprintf "je %s" label_after);
  emit_stmts block;
  e (sprintf "%s:" label_after)

and emit_while block node =
  let () = iter_counter := !iter_counter + 1 in
  let label_restart = sprintf "_while_restart_%d" !iter_counter in
  let label_out = sprintf "_while_out_%d" !iter_counter in
  e (sprintf "%s:" label_restart);
  emit_expr block node;
  e (sprintf "pop %s" rega);
  e (sprintf "cmp $0, %s" rega);
  e (sprintf "je %s" label_out);
  emit_stmts block;
  e (sprintf "jmp %s" label_restart);
  e (sprintf "%s:" label_out)

and emit_unary block node op =
  let res = match op with
    | Pos -> "" (* NOP *)
    | Neg -> sprintf "negq %s" rega
    | Odd -> sprintf "andq $1, %s" rega in
  if String.length res > 0 then
    begin
      emit_expr block node;
      e (sprintf "pop %s" rega);
      e res;
      e (sprintf "push %s" rega);
    end
  else ()

and emit_binary_cond op =
  e (sprintf "pop %s" rega);
  e (sprintf "pop %s" regb);
  e (sprintf "xorq %s, %s" regcompq regcompq);
  e (sprintf "cmpq %s, %s" rega regb);
  let sop = match op with
    | Eql -> "sete"
    | Neq -> "setne"
    | Lt -> "setl"
    | Gt -> "setg"
    | Leq -> "setle"
    | Geq -> "setge"
    | _ -> failwith "NOTREACHED" in
  e (sprintf "%s %s" sop regcomp);
  e (sprintf "push %s" regcompq)

and emit_binary_arith op =
  let push = ref regb in
  if op = Div then
    begin
      e (sprintf "pop %s" regb);
      e (sprintf "pop %s" rega);
    end
  else
    begin
      e (sprintf "pop %s" rega);
      e (sprintf "pop %s" regb);
    end;
  begin
    match op with
    | Add -> e (sprintf "addq %s, %s" rega regb)
    | Sub -> e (sprintf "subq %s, %s" rega regb)
    | Mul -> e (sprintf "imulq %s, %s" rega regb)
    | Div ->
      push := "%rax";
      e "cqo";
      e (sprintf "idivq %s" regb)
    | _ -> failwith "NOTREACHED"
  end;
  e (sprintf "push %s" !push)

and emit_binary block node_left node_right op =
  emit_expr block node_left;
  emit_expr block node_right;
  match op with
  | Add | Sub | Mul | Div -> emit_binary_arith op
  | Eql | Neq | Lt | Gt | Leq | Geq -> emit_binary_cond op

and emit_value value =
  e (sprintf "pushq $%d" (int_of_value value))

and emit_id block id =
  e (sprintf "pushq %s" (get_var_ref block id))

and emit_expr block = function
  | NodeUnary (n, op) -> emit_unary block n op
  | NodeBinary (nl, op, nr) -> emit_binary block nl nr op
  | NodeValue value -> emit_value value
  | NodeId id -> emit_id block id

and emit_assign block var node =
  let dest = get_var_ref block var in
  comment ("assign: " ^ var ^ " -> " ^ dest);
  emit_expr block node;
  e (sprintf "popq %s" dest);
  emit_value_print block var

(*
 * On assignments, we'll want to print the resulting value.
 *)
and emit_value_print block var =
  let src = get_var_ref block var in
  let name = get_var_name var in
  e "movq $0, %rax";
  e (sprintf "movq $%s, %%rsi" name);
  e (sprintf "movq %s, %%rdx" src);
  e (sprintf "movq $%s, %%rdi" pv_format);
  e "call printf"

(*
 * Our grammar supports nested procedures, but to *really* support them
 * we would have to decide the meaning of scope more carefully. We want to
 * support recursion, so easier to just, uh, not support nested procedures.
 * I don't think we'll do closures here.
 *)
let emit_procs block =
  comment "-- procs start --";
  List.iter
    (fun (proc, pblock) ->
       emit_proc_entry proc pblock;
       emit_stmts pblock;
       emit_proc_depart proc pblock)
    block.procs;
  comment "-- procs end --"

let emit_root block =
  emit_proc_entry ~do_stack:false "_opl_main" block;
  emit_stmts block;
  emit_proc_depart ~do_stack:false "_opl_main" block

let emit_entry block =
  emit_root block;
  emit_proc_entry ~do_stack:false "main" block;
  e "subq $8, %rsp";
  e "mov $_greetings, %rdi";
  e "call puts";
  e "call _opl_main";
  e "addq $8, %rsp";
  emit_proc_depart ~do_stack:false "main" block

let generate root =
  emit_header "codegen-test";
  emit_proc_decls root;
  e ".bss";
  emit_global_allocs root;
  e ".text";
  emit_common root;
  emit_procs root;
  emit_entry root;
  List.rev !output
