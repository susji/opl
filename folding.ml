open Ast

exception FoldingError of string

let tnote m = Printf.printf "[folding] %s\n" m
let int_of_bool b = if b then 1 else 0

(*
 * Assignments, output, and conditions contain arithmetic expressions.
 * We'll find all pure-constant terms and precalculate them. We'll treat
 * conditionals such that '0' is FALSE and anything else is TRUE.
 *
 * We also fold no-op expressions such as "x * 1" or "x + 0"
 *)

let rec eval_bin_op node  =
  tnote ("Evaluating " ^ (string_of_node node));
  let eval_int_op op a b =
    NodeValue (Int (op (int_of_value a) (int_of_value b)))
  and eval_bool_op op a b =
    NodeValue (Int (int_of_bool (op (int_of_value a) (int_of_value b))))
  in match node with
  | NodeBinary (lv, op, rv) ->
    tnote (Printf.sprintf "<%s %s %s>"
             (string_of_node' lv)
             (string_of_binary_op op)
             (string_of_node' rv));
    let tnode = match (op, lv, rv) with
      (* Explicit constant operations. *)
      | (Add, NodeValue lv', NodeValue rv') -> eval_int_op (+) lv' rv'
      | (Sub, NodeValue lv', NodeValue rv') -> eval_int_op (-) lv' rv'
      | (Mul, NodeValue lv', NodeValue rv') -> eval_int_op ( * ) lv' rv'
      | (Div, NodeValue lv', NodeValue rv') -> eval_int_op (/) lv' rv'
      | (Lt, NodeValue lv', NodeValue rv') -> eval_bool_op (<) lv' rv'
      | (Gt, NodeValue lv', NodeValue rv') -> eval_bool_op (>) lv' rv'
      | (Neq, NodeValue lv', NodeValue rv') -> eval_bool_op (<>) lv' rv'
      | (Eql, NodeValue lv', NodeValue rv') -> eval_bool_op (=) lv' rv'
      | (Leq, NodeValue lv', NodeValue rv') -> eval_bool_op (<=) lv' rv'
      | (Geq, NodeValue lv', NodeValue rv') -> eval_bool_op (>=) lv' rv'
      (* No-op arithmetics. *)
      | (Sub, NodeValue c, _)
      | (Add, NodeValue c, _) when int_of_value c = 0 -> rv
      | (Sub, _, NodeValue c)
      | (Add, _, NodeValue c) when int_of_value c = 0 -> lv
      | (Mul, NodeValue c, _)
      | (Div, NodeValue c, _) when int_of_value c = 1 -> rv
      | (Mul, _, NodeValue c)
      | (Div, _, NodeValue c) when int_of_value c = 1 -> lv
      (*
       * If we have nothing explicit to calculate, then we eval.
       * the child nodes and see if after recursing we have managed
       * to produce a pair of immediate values around the operator.
       *)
      | _ ->
        let (lv', _, rv') = (eval_node lv, op, eval_node rv) in
        match (lv', rv') with
        | (NodeValue _, NodeValue _) ->
          eval_bin_op (NodeBinary (lv', op, rv'))
        | _ -> NodeBinary (lv', op, rv')
    in tnode
  | _ -> node

and eval_un_op node  =
  tnote ("Evaluating " ^ (string_of_node node));
  let eval_int_op op v =
    NodeValue (Int (op (int_of_value v)))
  in match node with
  | NodeUnary (v, op) ->
    let tnode = match (op, v) with
      | (Pos, NodeValue v') -> eval_int_op (fun x -> x) v'
      | (Neg, NodeValue v') -> eval_int_op (~-) v'
      | (Odd, NodeValue v') -> eval_int_op (fun x -> x mod 2) v'
      | _ ->
        let v' = eval_node v in match v' with
        | NodeValue _ -> eval_un_op (NodeUnary (v', op))
        | _ -> NodeUnary (v', op)
    in tnode
  | _ -> node

and eval_node node =
  match node with
  | NodeUnary _ -> eval_un_op node
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

let rec fold_constants block =
  analyze_statements block;
  recurse_into_blocks block fold_constants