open Type

let rec substitution_in_term env term =
  match term with
  | Functor (name, term_list) -> Functor (name, substitution_in_term_list env term_list)
  | BuiltinFunctor (builtin_functor, term1, term2) -> BuiltinFunctor (builtin_functor, substitution_in_term env term1, substitution_in_term env term2)
  | Integer valu -> Integer valu
  | EmptyList -> EmptyList
  | Cons (term1, term2) -> Cons (substitution_in_term env term1, substitution_in_term env term2)
  | Variable (name, id, line) -> try substitution_in_term env (Env.find env id)
    with Not_found -> Variable (name, id, line)
  
and substitution_in_term_list env term_list =
  match term_list with
  | [] -> []
  | head :: tail -> substitution_in_term env head :: substitution_in_term_list env tail

let substitution_in_builtin_predicate env = function
| Is(t1, t2) -> Is(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticEquality(t1, t2) -> ArithmeticEquality(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticInequality(t1, t2) -> ArithmeticInequality(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticLess(t1, t2) -> ArithmeticLess(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticGreater(t1, t2) -> ArithmeticGreater(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticLeq(t1, t2) -> ArithmeticLeq(substitution_in_term env t1, substitution_in_term env t2)
| ArithmeticGeq(t1, t2) -> ArithmeticGeq(substitution_in_term env t1, substitution_in_term env t2)
| TermEquality(t1, t2) -> TermEquality(substitution_in_term env t1, substitution_in_term env t2)
| TermInequality(t1, t2) -> TermInequality(substitution_in_term env t1, substitution_in_term env t2)
| TermUnify(t1, t2) -> TermUnify(substitution_in_term env t1, substitution_in_term env t2)
| TermNotUnify(t1, t2) -> TermNotUnify(substitution_in_term env t1, substitution_in_term env t2)
| TermVar t -> TermVar(substitution_in_term env t)
| TermNotVar t -> TermNotVar(substitution_in_term env t)
| TermInteger t -> TermInteger(substitution_in_term env t)
| TermNotInteger t -> TermNotInteger(substitution_in_term env t)

let substitution_in_predicate env = function
  | Predicate(id, term_list) -> Predicate(id, List.map (substitution_in_term env) term_list)
  | BuiltinPredicate(builtin) -> BuiltinPredicate(substitution_in_builtin_predicate env builtin)
