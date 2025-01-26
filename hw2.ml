type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let convert_grammar graml = 
  let (start, rules) = graml in 
  let rule_map = 
    List.fold_left (fun acc (lhs, rhs) -> 
      match List.assoc_opt lhs acc with
      | None -> (lhs, [rhs]) :: acc
      | Some existing -> (lhs, existing @ [rhs]) :: acc
    ) [] rules
  in
  (start, fun nt ->
    match List.assoc_opt nt rule_map with
    | Some expansions -> expansions
    | None -> [])

let parse_tree_leaves tree =
  let rec helper acc tree =
    match tree with
    | Leaf terminal -> terminal :: acc
    | Node (_, children) -> List.fold_left helper acc children
  in
  List.rev (helper [] tree)