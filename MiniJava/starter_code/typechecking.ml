open LMJ

exception Error of string

let error msg pos1 pos2 =
  raise(Error(Printf.sprintf "%s\n%s" (Error.positions pos1 pos2) msg))

let rec typecheck_expression (exp : expression) =
  match Location.content exp with
  | EConst e -> ()
  | _ -> error "kermit" (Location.startpos exp) (Location.endpos exp)

let rec typecheck_instruction (ins : instruction) =
  match ins with
  | ISyso i -> typecheck_expression i
  | _ -> () (*todo*)

let typecheck_program (p : program) =
    typecheck_instruction p.main
