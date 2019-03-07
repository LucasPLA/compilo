open LMJ
open Printf

type method_type = typ list * typ

type method_env = method_type StringMap.t

type attribute_env = typ StringMap.t

type class_type = attribute_env * method_env

type class_env = class_type StringMap.t

type variable_env = typ StringMap.t

exception Error of string

let error (location : 'a Location.t) (msg : string) =
  raise (Error (sprintf "%s:\n%s"
                  (Error.positions (Location.startpos location) (Location.endpos location))
                  msg))

let errors (locations : 'a Location.t list) (msg : string) =
  raise (Error (sprintf "%s%s"
                  (List.fold_right (fun location acc ->
                      sprintf "%s:\n%s" (Error.positions (Location.startpos location) (Location.endpos location)) acc
                   ) locations "") msg))

let lookup (thing : string) (i : identifier) (env : 'a StringMap.t) =
  try
    StringMap.find (Location.content i) env
  with Not_found ->
    error i (sprintf "%s %s is undefined" thing (Location.content i))

let vlookup : identifier -> variable_env -> typ = lookup "variable"

let mlookup : identifier -> method_env -> method_type = lookup "method"

let alookup : identifier -> attribute_env -> typ = lookup "attribute"

let clookup : identifier -> class_env -> class_type = lookup "class"

let rec equal (typ1 : typ) (typ2 : typ) (instanceof : identifier -> identifier -> bool) : bool =
  match typ1, typ2 with
  | TypInt, TypInt
  | TypBool, TypBool
  | TypIntArray, TypIntArray -> true
  | Typ t1, Typ t2 -> instanceof t2 t1
  | _, _ -> false

let rec print_type : typ -> string = function
  | TypInt -> "integer"
  | TypBool -> "boolean"
  | TypIntArray -> "int[]"
  | Typ t -> Location.content t

let rec typecheck_call (cenv : class_env) (venv : variable_env)
    (instanceof : identifier -> identifier -> bool)
    (o : expression)
    (callee : identifier)
    (expressions : expression list) : typ =
  let o_type = typecheck_expression cenv venv instanceof o in
  match o_type with
  | Typ t ->
    begin
      let _, method_env = clookup t cenv in
      let (formals : typ list), (result : typ) = mlookup callee method_env in
      try
        List.iter2 (typecheck_expression_expecting cenv venv instanceof) formals expressions;
        result
      with Invalid_argument _ ->
        error callee
          (sprintf "Invalid function call, expected %d arguments, got %d"
             (List.length formals)
             (List.length expressions))
    end
  | _ -> error o (sprintf "A class is expected, got %s" (print_type o_type))


and typecheck_expression_expecting (cenv : class_env) (venv : variable_env)
    (instanceof : identifier -> identifier -> bool)
    (typ1 : typ)
    (e : expression) : unit =
  let typ2 = typecheck_expression cenv venv instanceof e in
  if not (equal typ1 typ2 instanceof) then
    error e
      (sprintf "Type mismatch, expected %s, got %s" (print_type typ1) (print_type typ2))

and typecheck_expression (cenv : class_env) (venv : variable_env)
    (instanceof : identifier -> identifier -> bool)
    (e : expression) : typ =
  match Location.content e with
  | EConst (ConstBool _) -> TypBool

  | EConst (ConstInt _) -> TypInt

  | EGetVar v -> vlookup v venv

  | EUnOp (op, e) ->
      let expected, returned =
        match op with
        | UOpNot -> TypBool, TypBool
      in
      typecheck_expression_expecting cenv venv instanceof expected e;
      returned

  | EBinOp (op, e1, e2) ->
      let expected, returned =
        match op with
        | OpAdd
        | OpSub
        | OpMul -> TypInt, TypInt
        | OpLt  -> TypInt, TypBool
        | OpAnd -> TypBool, TypBool
      in
      typecheck_expression_expecting cenv venv instanceof expected e1;
      typecheck_expression_expecting cenv venv instanceof expected e2;
      returned

  | EMethodCall (o, callee, expressions) -> typecheck_call cenv venv instanceof o callee expressions

  | EArrayGet (earray, eindex) ->
    typecheck_expression_expecting cenv venv instanceof TypInt eindex;
    typecheck_expression_expecting cenv venv instanceof TypIntArray earray;
    TypInt

  | EArrayAlloc elength ->
    typecheck_expression_expecting cenv venv instanceof TypInt elength;
    TypIntArray

  | EArrayLength earray ->
    typecheck_expression_expecting cenv venv instanceof TypIntArray earray;
    TypInt

  | EThis -> vlookup (Location.make (Location.startpos e) (Location.endpos e) "this") venv

  | EObjectAlloc id ->
      clookup id cenv |> ignore;
      Typ id

let rec typecheck_instruction (cenv : class_env) (venv : variable_env)
    (instanceof : identifier -> identifier -> bool)
    (inst : instruction) : unit =
  match inst with
  | ISetVar (v, e) ->
    typecheck_expression_expecting cenv venv instanceof (vlookup v venv) e

  | IArraySet (earray, eindex, evalue) ->
    typecheck_expression_expecting cenv venv instanceof TypIntArray
      (Location.make (Location.startpos earray) (Location.endpos earray) (EGetVar earray));
    typecheck_expression_expecting cenv venv instanceof TypInt eindex;
    typecheck_expression_expecting cenv venv instanceof TypInt evalue

  | IBlock instructions ->
    List.iter (typecheck_instruction cenv venv instanceof) instructions

  | IIf (cond, ithen, ielse) ->
    typecheck_expression_expecting cenv venv instanceof TypBool cond;
    typecheck_instruction cenv venv instanceof ithen;
    typecheck_instruction cenv venv instanceof ielse

  | IWhile (cond, ibody) ->
    typecheck_expression_expecting cenv venv instanceof TypBool cond;
    typecheck_instruction cenv venv instanceof ibody

  | ISyso e ->
    typecheck_expression_expecting cenv venv instanceof TypInt e

let occurrences (x : string) (bindings : (identifier * 'a) list) : identifier list =
  List.map fst (List.filter (fun (id, _) -> x = Location.content id) bindings)

let map_of_association_list (entity : string) (bindings : (identifier * 'a) list) : 'a StringMap.t =
  try
    StringMap.of_association_list (List.map (fun (id, data) -> (Location.content id, data)) bindings)
  with StringMap.Duplicate x ->
    errors (occurrences x bindings) (sprintf "%s %s is declared more than once" entity x)

let variable_map (bindings : (identifier * typ) list) : variable_env =
  map_of_association_list "Variable" bindings

let method_map (decls : (identifier * method_type) list) : method_env =
  map_of_association_list "Method" decls

let typecheck_method (cenv : class_env) (venv : variable_env)
    (instanceof : identifier -> identifier -> bool)
    (m : metho) : unit =

  let formals = m.formals
  and locals = m.locals in

  let mformals = variable_map formals
  and mlocals = variable_map locals in

  begin
    try
      let x =
        StringSet.choose
          (StringSet.inter
             (StringMap.domain mformals)
             (StringMap.domain mlocals))
      in
      errors (occurrences x formals @ occurrences x locals)
        "A formal parameter and a local variable cannot carry the same name"
    with Not_found ->
      ()
  end;

  let venv =
    StringMap.addm mformals venv
  |> StringMap.addm mlocals
  in

  typecheck_instruction cenv venv instanceof m.body;
  typecheck_expression_expecting cenv venv instanceof m.result m.return

let typecheck_class (cenv : class_env) (instanceof : identifier -> identifier -> bool)
    ((name, c) : identifier * clas) : unit =
  let attribute_env, _ = clookup name cenv in
  let venv = StringMap.add "this" (Typ name) attribute_env in
  List.iter (typecheck_method cenv venv instanceof) (List.map snd c.methods)

let extract_method_type (m : metho) : method_type =
  (List.map snd m.formals, m.result)

let extract_class_type (c : clas) : class_type =
  (variable_map c.attributes, method_map (List.map (fun (id, m) -> (id, extract_method_type m)) c.methods))

let class_map (decls : (identifier * clas) list) : clas StringMap.t =
  map_of_association_list "Class" decls

let create_instanceof (cmap : clas StringMap.t) : identifier -> identifier -> bool =
  let rec instanceof id1 id2 =
    if id1 = id2 then true
    else
      try
        match (StringMap.find id1 cmap).extends with
        | None -> false
        | Some id3 -> instanceof (Location.content id3) id2
      with Not_found -> false
  in
  fun id1 id2 ->
    instanceof (Location.content id1) (Location.content id2)
  (* let memo = Hashtbl.create 97 in *)
  (* fun id1 id2 -> *)
  (*   let id1', id2' = Location.content id1, Location.content id2 in *)
  (*   try *)
  (*     Hashtbl.find memo (id1', id2') *)
  (*   with Not_found -> *)
  (*     let res = instanceof id1' id2' in *)
  (*     Hashtbl.add memo (id1', id2') res; *)
  (*     res *)

let add_method (cmap : clas StringMap.t) (instanceof : identifier -> identifier -> bool) : clas StringMap.t =
  let test_compatible_signature ((name, m) : identifier * metho) ((name', m') : identifier * metho) : unit =
    let typecheck_params (typ : typ) (typ' : typ) : unit =
      if not (equal typ typ'
                (fun t1 t2 -> Location.content t1 = Location.content t2))
      then
        errors [name; name']
          (sprintf "Type mismatch in params of overriden method, expected %s, got %s" (print_type typ) (print_type typ'))
    in
    let typecheck_result (typ : typ) (typ' : typ) : unit =
      if not (equal typ typ' instanceof) then
          errors [name; name']
            (sprintf "Type mismatch in result of overriden method, expected %s, got %s" (print_type typ) (print_type typ'))
    in
    let formals, result = extract_method_type m
    and formals', result' = extract_method_type m' in
    try
      List.iter2 typecheck_params formals formals';
      typecheck_result result result'
    with Invalid_argument _ ->
      errors [name; name']
        (sprintf "A function that overrides another one must have the same number of parameters" )
  in
  (*
    The call to 'complete o c' adds to the class 'c' all methods and attributes of its parents classes.
    It checks if an overriden method (a method already defined with the same name in a parent class)
    is correctly typed: same parameters and a return type that is compatible with the overriden method.
    When there exists attributes with the same name in a parent class, we only keep the ones from the subclass.
  *)
  let rec complete (o : identifier option) (c : clas) : clas =
    match o with
    | None -> c
    | Some id ->
      let c' = StringMap.find (Location.content id) cmap in
      complete c'.extends
        {
          c with
            attributes =
            (List.filter
               (fun (name, _) ->
                 not (List.exists (fun (name', _) -> Location.content name = Location.content name') c.attributes)
               )
               c'.attributes) @ c.attributes;

            methods =
            (List.filter
               (fun (name, m) ->
                 try
                   List.find (fun (name', _) -> Location.content name = Location.content name') c.methods
                   |> test_compatible_signature (name, m);
                   false
                 with Not_found -> true
               )
               c'.methods) @ c.methods
        }
  in
  StringMap.map
    (fun c -> complete c.extends c)
    cmap

let typecheck_program (p : program) : unit =
  let cmap = class_map p.defs in
  let instanceof = create_instanceof cmap in
  let cenv =
    add_method cmap instanceof
      |> StringMap.map extract_class_type
  in
  List.iter (typecheck_class cenv instanceof) p.defs;
  let venv = StringMap.singleton "this" (Typ p.name) in
  typecheck_instruction cenv venv instanceof p.main
