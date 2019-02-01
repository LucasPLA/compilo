open Type

let rec get_var_from_term (term : term) : (string * int * int) list =
  match term with
  | Integer _ | EmptyList -> []
  | Variable (name, id, line) -> [(name, id, line)]
  | Functor (_name, term_list) -> get_var_from_termlist term_list
  | BuiltinFunctor (_functor, term1, term2) -> get_var_from_term term1 @ get_var_from_term term2
  | Cons (term1, term2) -> get_var_from_term term1 @ get_var_from_term term2

and get_var_from_termlist (terms : term list) : (string * int * int) list =
  match terms with
  | [] -> []
  | head :: tails -> get_var_from_term head @ get_var_from_termlist tails

let get_var_from_builtin (builtin : builtin_predicate) : (string * int * int) list =
  match builtin with
  | Is (term1, term2) 
  | ArithmeticEquality (term1, term2)
  | ArithmeticInequality (term1, term2)
  | ArithmeticLess (term1, term2)
  | ArithmeticGreater (term1, term2)
  | ArithmeticLeq (term1, term2)
  | ArithmeticGeq (term1, term2)
  | TermEquality (term1, term2)
  | TermInequality (term1, term2)
  | TermUnify (term1, term2)
  | TermNotUnify (term1, term2) -> get_var_from_term term1 @ get_var_from_term term2
  | TermVar term 
  | TermNotVar term 
  | TermInteger term 
  | TermNotInteger term -> get_var_from_term term
  

let get_variables_from_predicate (predicate : predicate) : (string * int * int) list = 
  match predicate with
  | Predicate (_, terms) -> get_var_from_termlist terms
  | BuiltinPredicate builtin -> get_var_from_builtin builtin

let check_clause = function
  | Clause(predicate, predicate_list) ->
    let variables =
      predicate :: predicate_list
      |> List.map get_variables_from_predicate
      |> List.concat
      |> List.sort Pervasives.compare
    in
    let print_error (pos, x) =
      Printf.printf "singleton variable %s at line %d\n" x pos
    in
    let rec singleton_variable prev = function
      | [] -> []
      | (x, _, _) :: r when String.get x 0 = '_' -> singleton_variable None r
      | (x1, id1, pos1) :: (x2, id2, pos2) :: r when id1 = id2 -> singleton_variable (Some id1) r
      | (x, id, pos) :: r when prev = Some id -> singleton_variable prev r
      | (x, id, pos) :: r -> (pos, x) :: singleton_variable (Some id) r
    in
    singleton_variable None variables
    |> List.sort Pervasives.compare
    |> List.iter print_error

let check_program prog = List.iter check_clause prog
