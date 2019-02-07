open Type

exception Not_unifiable

let rec unify_terms env t1 t2 =
  match Substitution.substitution_in_term env t1, Substitution.substitution_in_term env t2 with
  | (Functor (st1, term_list1), Functor (st2, term_list2)) ->
        if st1 = st2 then unify_lists env term_list1 term_list2 else raise Not_unifiable
  | (Integer i1, Integer i2) -> if i1 = i2 then env else raise Not_unifiable
  | (BuiltinFunctor (b1, t11, t12), BuiltinFunctor(b2, t21, t22)) ->
        if b1 = b2 then unify_terms (unify_terms env t11 t21) t12 t22 else raise Not_unifiable
  | (Cons (t11, t12), Cons (t21, t22)) -> unify_terms (unify_terms env t11 t21) t12 t22
  | (EmptyList, EmptyList) -> env
  | (Variable (string, i, line), x) -> Env.add env i x
  | (x, Variable (string, i, line)) -> Env.add env i x
  | _ -> raise Not_unifiable

and unify_lists env l1 l2 =
  try
    List.fold_left2 (fun env t1 t2 -> unify_terms env t1 t2) env l1 l2
  with Invalid_argument _ -> raise Not_unifiable

let unify_predicates env = function
  | Predicate(p1, l1), Predicate(p2, l2) when p1 = p2 -> unify_lists env l1 l2
  | _ -> raise Not_unifiable
