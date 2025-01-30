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
(*let make_matcher gram = 
  let (start_symbol, rule_function) = gram in
  fun accept frag ->
    let rec match_prefix rules frag =
      match rules with
      | [] -> None
      | rule :: rest_rules ->
          (match match_rule rule frag with
          | Some suffix -> Some suffix   
          | None -> match_prefix rest_rules frag) 
    and match_rule rule frag =
      match (rule, frag) with
      | ([], suffix) -> Some suffix
      | ((T term)::rest_rule, t'::ts) ->
          if term = t' then match_rule rest_rule ts
          else None
      | ((N nonterm)::rest_rule, _) ->
          (match match_prefix (rule_function nonterm) frag with
          | None -> None
          | Some suffix -> match_rule rest_rule suffix)
      | _, [] -> None
    in
    match match_prefix (rule_function start_symbol) frag with
    | None -> None
    | Some suffix -> accept suffix *)

let make_matcher gram = 
    let (start_symbol, rule_function) = gram in
    fun accept frag -> 
    (*breaks all the rules into individual rules that are matched with frag*)
    let rec match_prefix rules frag = 
      match rules with 
      | [] -> None (* if there are no rules, then no match*)
      | rule :: rest_rules -> (*break down the rules into rule *)
        (match match_rule rule frag with 
        | None -> match_prefix rest_rules frag (*if match_rule returns None, it means either the somewhere the rule was not met*)
        | Some suffix -> accept suffix) (* if matchrule returns a suffix, it means that the rule was ran all the way 
        through and the suffix is the remaining fragment*)
(*matches a single rule with a fragment*)
      and match_rule rule frag = 
      (match (rule, frag) with
      | ([], suffix) -> Some suffix 
      | ((T term)::rest_terms, t'::ts) -> 
          if term = t' then match_rule rest_terms ts
          else None 
      | ((N nonterm)::rest_terms, frag) ->
          (match match_prefix (rule_function nonterm) frag with 
          | None -> None
          | Some suffix -> match_rule rest_terms suffix)
      | (_, []) -> None)
    in 
(* tries to match a prefix with a rule and if it returns None, no prefix was matched
if it returns some, then it should call the accept function with the suffix*)
    match match_prefix (rule_function start_symbol) frag with
    | None -> None
    | Some suffix -> accept suffix 
(*make_parser uses the same logic as make matcher but it concatenates the trees the come out of it*)
let make_parser gram =
  let (start_symbol, rule_function) = gram in
  fun frag ->
    let rec match_prefix rules frag =
      match rules with (*breaks down rules into rule to we can check one by one*)
      | [] -> None
      | rule :: rest_rules ->
          match match_rule rule frag with
          | None -> match_prefix rest_rules frag
          | Some (tree_list, suffix) ->
              if suffix = [] then Some (Node (start_symbol, tree_list))
              else match_prefix rest_rules frag

    and match_rule rule frag = (*checks one rule with the frag and depending on the status of the rule it will recursively call try_rules in order to "explore" that path*)
      (*if the path fails in the try_rules, it returns back out one level and tries the next rule*)
      match (rule, frag) with
      | ([], suffix) -> Some ([], suffix)
      | ((T term) :: rest_terms, t' :: ts) ->
          if term = t' then
            match match_rule rest_terms ts with
            | None -> None
            | Some (tree_list, suffix) -> Some (Leaf t' :: tree_list, suffix)
            (*this keeps track of the tree list as it keeps getting recursively built up
            expr -> term -> lvalue, incrop -> ...*)
          else None
          (*this case takes care of nonterminals leading to other nonterminals*)
          (*this should recursively call match_rule on nonterminals until it reaches a terminal where it returns the list*)
      | ((N nonterm) :: rest_terms, _) ->
          let rules = rule_function nonterm in
          let rec try_rules rules frag = (*uses the rule from the nonterminal just seen*)
            match rules with
            | [] -> None
            | rule :: rest_rules ->
                match match_rule rule frag with
                | None -> try_rules rest_rules frag (* if the rule does not match keep going*)
                | Some (subtree_list, inner_suffix) -> 
                    match match_rule rest_terms inner_suffix with (*matches with the rest of the rules if the first expansion into the nonterminal was successful*)
                    | None -> try_rules rest_rules frag 
                    (*this is the recursive call where it keeps exploring the rest of the rules given that the nonterminal was successful*)
                    | Some (rest_tree_list, final_suffix) ->
                        Some (Node (nonterm, subtree_list) :: rest_tree_list, final_suffix) 
                        (*same logic as matcher but instead returns a tree that keeps gettig added to*)
          in
          try_rules rules frag
      | (_, []) -> None
    in
    match match_prefix (rule_function start_symbol) frag with
    | None -> None
    | Some tree -> Some tree
