open Printf
open Print
open MJ

let constant () (c : MJ.constant) : string =
  match c with
  | ConstBool true -> sprintf "1"
  | ConstBool false -> sprintf "0"
  | ConstInt i -> sprintf "%ld" i


let binop (op : MJ.binop) : string =
  match op with
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpLt  -> "<"
  | OpAnd -> "&&"


let print_type (struct_array_name : string) (typ : MJ.typ) : string =
  match typ with
  | TypInt -> "int"
  | TypBool -> "int"
  | TypIntArray -> sprintf "struct %s*" struct_array_name
  | Typ t -> sprintf "struct %s*" t


let print_cast (struct_array_name : string) (typ : MJ.typ) : string = sprintf "(%s)" (print_type struct_array_name typ)


let indentation = 2


let rec get_class (var2class : string -> string) (method2index_and_type : string -> string -> int * MJ.typ) (e : MJ.expression) : string =
  match e with
  | EGetVar x -> var2class x

  | EMethodCall (o, m, _) ->
    begin
      match method2index_and_type (get_class var2class method2index_and_type o) m |> snd with
      | Typ t -> t
      | _ -> ""
    end

  | EThis -> var2class "this"

  | EObjectAlloc id -> id

  | _ -> ""


let rec expr2c
    ((struct_array_name, var2class, (var2c : string -> string -> string), method2index_and_type) as params)
    ()
    (expr : MJ.expression)
    : string =
  match expr with
  | EConst c -> sprintf "%a" constant c

  | EGetVar x -> var2c (var2class "this") x

  | EThis -> "this"

  | EMethodCall (o, c, es) ->
    let clas = get_class var2class method2index_and_type o in
    let index, typ = method2index_and_type clas c in
    sprintf "({ struct %s* tmp = %a; %s tmp->vtable[%d](tmp%a); })"
      clas
      (expr2c params) o
      (print_cast struct_array_name typ)
      index
      (preclist comma (expr2c params)) es

  | EArrayAlloc e ->
    let length = expr2c params () e in
    sprintf "(void*)({ struct %s* tmp = (struct %s*) malloc(sizeof(struct %s)); \
                       tmp->array = (int*) calloc(%s, sizeof(int)); \
                       tmp->length = %s; tmp; })"
      struct_array_name
      struct_array_name
      struct_array_name
      length
      length

  | EObjectAlloc id -> sprintf "({ struct %s* tmp = (struct %s*) calloc(1, sizeof(*tmp)); \
                                   tmp->vtable = %s_vtable; \
                                   tmp; })" id id id

  | EArrayGet (ea, ei) -> sprintf "(%a)->array[%a]" (expr2c params) ea (expr2c params) ei

  | EArrayLength e -> sprintf "(%a)->length" (expr2c params) e

  | EUnOp (UOpNot, e) -> sprintf "!(%a)" (expr2c params) e

  | EBinOp (op, e1, e2) -> sprintf "(%a %s %a)" (expr2c params) e1 (binop op) (expr2c params) e2


let rec instr2c ((struct_array_name, var2class, var2c, method2index_and_type) as params) () = function
  | ISetVar (x, e) ->
    let x_class = var2class x in
    let e_class = get_class var2class method2index_and_type e in
    sprintf "%s = %s%a;"
      (var2c (var2class "this") x)
      (if x_class <> e_class then sprintf "(struct %s*) " x_class else "")
      (expr2c params) e

  | IArraySet (id, ei, ev) -> sprintf "(%s)->array[%a] = %a;" (var2c (var2class "this") id) (expr2c params) ei (expr2c params) ev

  | IIf (c, i1, i2) ->
      sprintf "if (%a) %a%telse %a"
        (expr2c params) c
        (instr2c params) i1
        nl
        (instr2c params) i2

  | IWhile (c, i) ->
      sprintf "while (%a) %a"
        (expr2c params) c
        (instr2c params) i

  | IBlock is -> sprintf "{%a%t}" (indent indentation (seplist nl (instr2c params))) is nl

  | ISyso e -> sprintf "printf(\"%%d\\n\", %a);" (expr2c params) e

let rec fold_class_hierarchy
    (f : string -> MJ.clas -> 'a -> 'a)
    (defs : MJ.clas StringMap.t)
    (name_option : string option)
    (acc : 'a)
    : 'a =
  match name_option with
  | None -> acc
  | Some name ->
    let c = StringMap.find name defs in
    f name c (fold_class_hierarchy f defs c.extends acc)

(* let rec fold_class_hierarchy_memo memo f defs name_option acc = *)
(*   match name_option with *)
(*   | None -> acc *)
(*   | Some name -> *)
(*     try *)
(*       let res = Hashtbl.find memo name in *)
(*       printf "hit %s\n" name; *)
(*       res *)
(*     with Not_found -> *)
(*       let c = StringMap.find name defs in *)
(*       let res = f name c (fold_class_hierarchy_memo memo f defs c.extends acc) in *)
(*       Hashtbl.add memo name res; *)
(*       res *)


let class_declaration () class_name =
  sprintf "struct %s;"
    class_name


let print_decl struct_array_name () ((id, t) : string * MJ.typ) : string =
  sprintf "%s %s"
    (print_type struct_array_name t)
    id


let method_declaration struct_array_name () ((class_name, clas) : string * MJ.clas) : string =
  (seplist nl
     (fun () ((method_name, m) : string * MJ.metho) ->
       sprintf "void* %s_%s(struct %s* this%a);"
         class_name
         method_name
         class_name
         (preclist comma (print_decl struct_array_name)) m.formals))
    ()
    (StringMap.to_association_list clas.methods)


let class_definition struct_array_name (defs : MJ.clas StringMap.t) () (class_name, clas) : string =
  sprintf "struct %s {%t%a\n};"
    class_name

    (indent' indentation (fun () -> "void* (**vtable)();"))

    (termlist semicolon (indent indentation (fun () ((index, t) : int * MJ.typ) -> sprintf "%s _%d" (print_type struct_array_name t) index)))
    (fold_class_hierarchy
       (fun name c acc ->
           let n = ref (List.length acc + 1) in
           (StringMap.to_association_list c.attributes
               |> List.map (fun (_, t) -> let index = !n in incr n; (index, t)))
           :: acc
       )
       defs
       (Some class_name)
       []
        |> List.rev
        |> List.concat)


let method_definition struct_array_name defs var2c method2index_and_type () (class_name, clas) =
  (seplist nl
    (fun () (method_name, m) ->
      let (all_attributes : MJ.typ StringMap.t) =
        fold_class_hierarchy
          (fun _ clas acc ->
            StringMap.addm clas.attributes acc
          )
          defs
          (Some class_name)
          StringMap.empty
      in
      let (var2class : string -> string) =
        let (map : MJ.typ StringMap.t) =
          StringMap.addm
            (StringMap.addm m.locals (StringMap.of_association_list m.formals))
            (StringMap.add "this" (Typ class_name) all_attributes)
        in
        fun x ->
          match StringMap.find x map with
          | Typ t -> t
          | _ -> ""
      in
      let (visible_attributes : StringSet.t) =
        StringSet.diff
          (StringSet.diff (StringMap.domain all_attributes) (StringMap.domain m.locals))
          (StringSet.of_list (List.map fst m.formals))
      in
      let (is_attribute : string -> bool) =
        fun x -> StringSet.mem x visible_attributes
      in
      sprintf "void* %s_%s(struct %s* this%a) {%a%a%a\n}"
        class_name

        method_name

        class_name

        (preclist comma (print_decl struct_array_name))
        m.formals

        (termlist semicolon (indent indentation (print_decl struct_array_name)))
        (StringMap.to_association_list m.locals)

        (
          fun () i ->
            match i with
            | IBlock is ->
              (list (indent indentation (instr2c (struct_array_name, var2class, (var2c is_attribute), method2index_and_type)))) () is
            | _ -> assert false
        )
        m.body

        (indent indentation
           (
             fun () e ->
               sprintf "return (void*)(%a);" (expr2c (struct_array_name, var2class, (var2c is_attribute), method2index_and_type)) e
           )
        )
        m.return
    ))
    ()
    (StringMap.to_association_list clas.methods)


let vtable_definition () (class_name, (method_name2class_name_and_index_and_type : (string * int * MJ.typ) StringMap.t)) : string =
  let method_name_list =
    StringMap.to_association_list method_name2class_name_and_index_and_type
  |> List.sort (fun (_, (_, index1, _)) (_, (_, index2, _)) -> Pervasives.compare index1 index2)
  |> List.map (fun (method_name, (class_name, _, _)) -> sprintf "%s_%s" class_name method_name)
  in
  sprintf "void* (*%s_vtable[])() = {%a};"
    class_name
    (seplist comma (fun () s -> s)) method_name_list


let program2c (p : MJ.program) : unit =

  (*
    The map 'class_name2method_name2class_name_and_index_and_type' associates a class name to
    another map that associates a method name to a triplet:
      1) Name of the class where the method comes from, because a method can come from a parent class.
      2) The index of the method in the vtable.
      3) The result's type of the method.
  *)
  let (class_name2method_name2class_name_and_index_and_type : ((string * int * MJ.typ) StringMap.t) StringMap.t) =
    StringMap.mapi
      (
        fun class_name _ ->
          fold_class_hierarchy
            (fun class_name clas (acc : (string * int * MJ.typ) StringMap.t) ->
              let n = ref (StringMap.cardinal acc) in
              StringMap.fold
                (fun method_name m acc ->
                  if StringMap.mem method_name acc then begin
                    let class_name', index, _ = StringMap.find method_name acc in
                    StringMap.add method_name (class_name, index, m.result) acc
                  end
                  else begin
                    let index = !n in
                    incr n;
                    StringMap.add method_name (class_name, index, m.result) acc
                  end)
                clas.methods
                acc
            )
            p.defs
            (Some class_name)
            StringMap.empty
      )
      p.defs
  in

  let (class_name2method_name2index_and_type : ((int * MJ.typ) StringMap.t) StringMap.t) =
    StringMap.map (StringMap.map (fun (_, index, typ) -> (index, typ))) class_name2method_name2class_name_and_index_and_type
  in

  let method2index_and_type (class_name : string) (method_name : string) : (int * MJ.typ) =
    StringMap.find class_name class_name2method_name2index_and_type
  |> StringMap.find method_name
  in

  let pointer_size = 8 in

  let sizeof = function
  | TypInt -> 4
  | TypBool -> 4
  | TypIntArray -> pointer_size
  | Typ t -> pointer_size
  in

  (*
    I need the offset of an attribute (size) from the beginning of the struct because we can have the same
    name in a parent class, therefore I cannot access the attribute by its name in the corresponding struct.
  *)
  let (class_name2attribute_name2type_and_size : ((MJ.typ * int) StringMap.t) StringMap.t) =
    StringMap.mapi
      (
        fun class_name _ ->
          fold_class_hierarchy
            (fun _ clas (size, map) ->
              StringMap.fold
                (fun attribute_name typ (size, map) ->
                  (size + sizeof typ, StringMap.add attribute_name (typ, size) map)
                )
                clas.attributes
                (size, map)
            )
            p.defs
            (Some class_name)
            (pointer_size, StringMap.empty)
          |> snd
      )
      p.defs
  in

  let (defs : (string * clas) list) = StringMap.to_association_list p.defs in

  let rec variant s l =
    if List.mem s l then
      variant (s ^ "_") l
    else
      s
  in

  let struct_array_name = variant "array" (List.map fst defs) in

  let var2c (is_attribute : string -> bool) (this : string) (x : string) : string =
    if is_attribute x then
      let typ, size =
        StringMap.find this class_name2attribute_name2type_and_size
      |> StringMap.find x
      in
      sprintf "*((%s*)((char*) this + %d))" (print_type struct_array_name typ) size
    else x
  in

  Printf.fprintf stdout "%s%!"
    (
      sprintf
        "#include <stdio.h>\n\
         #include <stdlib.h>\n\
         #pragma GCC diagnostic ignored \"-Wpointer-to-int-cast\"\n\
         #pragma GCC diagnostic ignored \"-Wint-to-pointer-cast\"\n\
         struct %s { int* array; int length; };\n\
         %a\
         %a\
         %a\
         %a\
         %a\
         int main(int argc, char *argv[]) {\
         %a\
         %t\n\
         }\n"
        struct_array_name

        (termlist nl class_declaration)
        (List.map fst defs)

        (termlist nl (method_declaration struct_array_name))
        (List.filter (fun (_, clas) -> StringMap.cardinal clas.methods > 0) defs)

        (termlist nl (class_definition struct_array_name p.defs))
        defs

        (termlist nl vtable_definition)
        (StringMap.to_association_list class_name2method_name2class_name_and_index_and_type)

        (termlist nl (method_definition struct_array_name p.defs var2c method2index_and_type))
        (List.filter (fun (_, clas) -> StringMap.cardinal clas.methods > 0) defs)

        (indent indentation (instr2c (struct_array_name, (fun x -> if x = p.name then p.name else ""), var2c (fun _ -> false), method2index_and_type)))
        p.main

        (indent' indentation (fun () -> "return 0;"))
    )
