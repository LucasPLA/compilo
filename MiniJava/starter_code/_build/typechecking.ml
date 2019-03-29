open LMJ

exception Error of string

module VarMap = Map.Make(String)

let error msg pos1 pos2 =
  raise(Error(Printf.sprintf "%s\n%s" (Error.positions pos1 pos2) msg))

(*let rec typecheck_expected_expression (actual: expression) (expected: expression) =
  match Location.content actual with
  | Location.content expected -> ()
  | _ ->*) 

let rec typecheck_expression (exp : expression) varmap =
  match Location.content exp with
  | EConst e -> begin match e with
    | ConstBool _ -> TypBool
    | ConstInt _ -> TypInt
  end
  | EGetVar ident -> if VarMap.mem (Location.content ident) varmap then
    VarMap.find (Location.content ident) varmap
  else
    error "peggy" (Location.startpos exp) (Location.endpos exp)
  | EUnOp (op, exp) -> begin
    match (op, typecheck_expression exp varmap) with
    | (UOpNot, TypBool) -> TypBool
    | _ -> error "fozzy" (Location.startpos exp) (Location.endpos exp)
  end
  | EBinOp (op, exp1, exp2) -> begin 
    match (op, typecheck_expression exp1 varmap, typecheck_expression exp2 varmap) with
    | (OpAdd, TypInt, TypInt) -> TypInt
    | (OpSub, TypInt, TypInt) -> TypInt
    | (OpMul, TypInt, TypInt) -> TypInt
    | (OpLt, TypInt, TypInt) -> TypBool
    | (OpAnd, TypBool, TypBool) -> TypBool
    | _ -> error "fozzy" (Location.startpos exp) (Location.endpos exp)
  end
  | EArrayGet (arr, index) -> begin
    match (typecheck_expression arr varmap, typecheck_expression index varmap) with
    | (TypIntArray, TypInt) -> TypInt
    | _ -> error "fozzy" (Location.startpos exp) (Location.endpos exp)
  end
  | EArrayLength arr -> begin
    match typecheck_expression arr varmap with
    | TypIntArray-> TypInt
    | _ -> error "fozzy" (Location.startpos exp) (Location.endpos exp)
  end
  | _ -> error "kermit" (Location.startpos exp) (Location.endpos exp)

let rec typecheck_instruction (ins : instruction) varmap =
  match ins with
  | IBlock (inst_list) -> begin match inst_list with
    | [] -> ()
    | head::tail -> let _ = typecheck_instruction head varmap in typecheck_instruction (IBlock tail) varmap
  end
  | IIf (exp, ins1, ins2) -> begin match typecheck_expression exp varmap with
    | TypBool -> let _ = typecheck_instruction ins1 varmap and _ = typecheck_instruction ins2 varmap in ()
    | _ -> error "gonzo" (Location.startpos exp) (Location.endpos exp)
  end
  | IWhile (exp, ins) -> begin match typecheck_expression exp varmap with
    | TypBool -> typecheck_instruction ins varmap
    | _ -> error "gonzo" (Location.startpos exp) (Location.endpos exp)
  end
  | ISyso i -> let _ = typecheck_expression i varmap in ()
  | ISetVar (ident, exp) -> if VarMap.mem  (Location.content ident) varmap then
    begin match VarMap.find (Location.content ident) varmap with
      | t  when t = typecheck_expression exp varmap-> ()
      | _ -> error "gonzo" (Location.startpos exp) (Location.endpos exp)
    end
  else
    error "peggy" (Location.startpos exp) (Location.endpos exp)
  | IArraySet (ident, index, exp) -> if VarMap.mem  (Location.content ident) varmap then
    begin match (VarMap.find (Location.content ident) varmap, typecheck_expression index varmap, typecheck_expression exp varmap) with
      | (TypIntArray, TypInt, TypInt) -> ()
      | _ -> error "gonzo" (Location.startpos exp) (Location.endpos exp)
    end
  else
    error "peggy" (Location.startpos exp) (Location.endpos exp)

let typecheck_program (p : program) =
    typecheck_instruction p.main (VarMap.add (Location.content p.main_args) TypIntArray VarMap.empty) (*this is wrong, but there is no array of string (or generic array)*)

