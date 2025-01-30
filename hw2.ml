type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet

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
let make_matcher gram = 
  let (start_symbol, rule_function) = gram in
  fun accept frag ->
    let rec match_prefix rules frag =
      match rules with
      | [] -> None
      | rule :: rest_rules ->
          (match match_rule rule frag with
          | Some suffix -> Some suffix   (* Success with this rule *)
          | None -> match_prefix rest_rules frag) (* Try next rule *)
    and match_rule rule frag =
      match (rule, frag) with
      | ([], suffix) -> Some suffix
      | ((T term)::rest_rule, t'::ts) ->
          if term = t' then match_rule rest_rule ts
          else None
      | ((N nonterm)::rest_rule, _) ->
          (* Handle nonterminal by trying its productions *)
          (match match_prefix (rule_function nonterm) frag with
          | None -> None
          | Some suffix -> match_rule rest_rule suffix)
      | _, [] -> None
    in
    (* Final step after matching start symbol *)
    match match_prefix (rule_function start_symbol) frag with
    | None -> None
    | Some suffix -> accept suffix
  
