(*test cases provided in documentation*)
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type math_grammar_nonterminals =
  | Expr | Number | Op

let math_grammar =
  (Expr,
   function
    | Expr -> 
        [[N Number; N Op; N Number]]
    | Number ->
        [[T "1"];
         [T "2"];
         [T "3"]]
    | Op ->
        [[T "+"];
         [T "-"]]
  ) 

(* Test case that should pass *)
let make_matcher_test = 
  (make_matcher math_grammar accept_all ["1"; "+"; "2"]) = Some []

let make_parser_test = 
  match make_parser math_grammar ["1"; "+"; "2"] with
  | Some tree -> parse_tree_leaves(tree) = ["1"; "+"; "2"]
  | _ -> false


