open Ast

exception CSEError of string

let tnote m = Printf.printf "[comsubelim] %s\n" m
let int_of_bool b = if b then 1 else 0

(*
 * Search for common sub-expressions such as "x * 1 + x * 2" and
 * combine the common factors. In this case we would get "x * (1 + 2)"
 * which would further be constant-folded to "x * 3".
 *
 * As we operate only with integers, we won't do division.
 *)

let rec eval_bin_op node  =
  let () = tnote ("Evaluating " ^ (string_of_node node))
  in match node with
  | NodeBinary (lv, op, rv) ->
    tnote (Printf.sprintf "<%s %s %s>"
             (string_of_node' lv)
             (string_of_binary_op op)
             (string_of_node' rv));
    let tnode = match (op, lv, rv) with
      (*
       * (x * c1) + (x * c2)
       *)
      | (nop, NodeBinary (llv, Mul, lrv), NodeBinary (rlv, Mul, rrv)) ->
        if nop = Add || nop = Sub then
          match (llv, lrv, rlv, rrv) with
          | (NodeId il, nl, NodeId ir, nr)
          | (nl, NodeId il, NodeId ir, nr)
          | (NodeId il, nl, nr, NodeId ir)
          | (nl, NodeId il, nr, NodeId ir) when il = ir ->
            NodeBinary (NodeId il, Mul, NodeBinary (nl, op, nr))
          | _ -> node
        else
          node
      (*
       * x + (x * c)
       *)
      | (nop, NodeId li, NodeBinary (NodeId ri, Mul, nv))
      | (nop, NodeId li, NodeBinary (nv, Mul, NodeId ri))
      | (nop, NodeBinary (NodeId ri, Mul, nv), NodeId li)
      | (nop, NodeBinary (nv, Mul, NodeId ri), NodeId li) ->
        if (nop = Add || nop = Sub) && ri = li then
          NodeBinary (NodeId li, Mul, NodeBinary (NodeValue (Int 1), nop, nv))
        else
          node
      (*
       * No luck with the easy matches. Try recursing, if we manage
       * to create child nodes which might match on a new attempt.
       *)
      | _ ->
        let (lv', op', rv') = (eval_node lv, op, eval_node rv) in
        match (lv', rv') with
        | (NodeBinary _, NodeBinary _)
        | (NodeId _, NodeBinary _)
        | (NodeBinary _, NodeId _) when op = Add || op = Sub ->
          eval_bin_op (NodeBinary (lv', op, rv'))
        | _ -> NodeBinary (lv', op, rv')
    in tnode
  | _ -> node

and eval_node node =
  match node with
  | NodeBinary _ -> eval_bin_op node
  | _ -> node

let analyze_nodes = function
  | Assign (name, node) -> Assign (name, eval_node node)
  | While (node, block) -> While (eval_node node, block)
  | If (node, block) -> If (eval_node node, block)
  | Output node -> Output (eval_node node)
  | _ as stmt -> stmt

let analyze_statements block =
  block.stmts <- List.map analyze_nodes block.stmts

let rec eliminate_common_expr block =
  analyze_statements block;
  recurse_into_blocks block eliminate_common_expr