type token =
| INT of int
| VARIABLE of string
| NAME of string
| LEFT_PAREN | RIGHT_PAREN
| LEFT_BRACKET | RIGHT_BRACKET
| PIPE | DOT | COMMA | COLON_HYPHEN
| PLUS | MINUS | MULT | DIV
| TERM_EQ | TERM_INEQ | IS | TERM_UNIFY | TERM_NOT_UNIFY
| TERM_VAR | TERM_NOT_VAR | TERM_INTEGER | TERM_NOT_INTEGER
| ARITH_EQ | ARITH_INEQ | ARITH_LESS | ARITH_GREATER | ARITH_GEQ | ARITH_LEQ
| EOF

let print = function
  | INT i -> Printf.printf "INT %d\n" i
  | VARIABLE var -> Printf.printf "VARIABLE %s\n" var
  | NAME name -> Printf.printf "NAME %s\n" name
  | LEFT_PAREN -> Printf.printf "LEFT_PAREN\n"
  | RIGHT_PAREN -> Printf.printf "RIGHT_PAREN\n"
  | LEFT_BRACKET -> Printf.printf "LEFT_BRACKET\n"
  | RIGHT_BRACKET -> Printf.printf "RIGHT_BRACKET\n"
  | PIPE -> Printf.printf "PIPE\n"
  | DOT -> Printf.printf "DOT\n"
  | COMMA -> Printf.printf "COMMA\n"
  | COLON_HYPHEN -> Printf.printf "COLON_HYPHEN\n"
  | PLUS -> Printf.printf "PLUS\n"
  | MINUS -> Printf.printf "MINUS\n"
  | MULT -> Printf.printf "MULT\n"
  | DIV -> Printf.printf "DIV\n"

  | TERM_EQ -> Printf.printf "TERM_EQ\n"
  | TERM_INEQ -> Printf.printf "TERM_INEQ\n"
  | IS -> Printf.printf "IS\n"
  | TERM_UNIFY -> Printf.printf "TERM_UNIFY\n"
  | TERM_NOT_UNIFY -> Printf.printf "TERM_NOT_UNIFY\n"
  | TERM_VAR -> Printf.printf "TERM_VAR\n"
  | TERM_NOT_VAR -> Printf.printf "TERM_NOT_VAR\n"
  | TERM_INTEGER -> Printf.printf "TERM_INTEGER\n"
  | TERM_NOT_INTEGER -> Printf.printf "TERM_NOT_INTEGER\n"
  | ARITH_EQ -> Printf.printf "ARITH_EQ\n"
  | ARITH_INEQ -> Printf.printf "ARITH_INEQ\n"
  | ARITH_LESS -> Printf.printf "ARITH_LESS\n"
  | ARITH_GREATER -> Printf.printf "ARITH_GREATER\n"
  | ARITH_GEQ -> Printf.printf "ARITH_GEQ\n"
  | ARITH_LEQ -> Printf.printf "ARITH_LEQ\n"
  | EOF -> Printf.printf "EOF\n"

exception Lexical_error of string

let line_number = ref 0

let newline () = incr line_number

let error msg = raise (Lexical_error (msg ^ " at line " ^ string_of_int !line_number))

let rec build_int = fun (stream, prev) -> match Stream.peek stream with
            | Some('0' .. '9' as digit) -> Stream.junk stream |> ignore; build_int (stream, (prev*10 + int_of_string (String.make 1 digit)))
            | _ -> prev

let rec build_string = fun (stream, prev) -> match Stream.peek stream with
  | Some('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' as letter) -> Stream.junk stream |> ignore;
                                          build_string (stream, prev ^ String.make 1 letter)
  | _ -> prev
 
let rec get_token stream =
  try
    begin
      let next () = Stream.next stream in
      let peek () = Stream.peek stream in
      let junk () = Stream.junk stream |> ignore in
      let char_to_string c = String.make 1 c in
      match next() with
        | '+' -> PLUS
        | '-' -> MINUS
        | '*' -> MULT
        | '%' -> DIV
        | '.' -> DOT
        | ',' -> COMMA
        | '0' .. '9' as digit -> INT (build_int (stream, int_of_string (char_to_string digit)))
        | 'a' .. 'z' as letter -> NAME (build_string (stream, char_to_string letter))
        | 'A' .. 'Z' | '_' as letter -> VARIABLE (build_string (stream, char_to_string letter))
        | '=' -> begin
          match peek() with
            | Some('=') -> junk(); TERM_EQ
            | Some(':') -> junk(); if Some('=') = peek() then begin junk(); ARITH_EQ  end else begin error "invalid token 1" end
            | Some('>') -> junk(); ARITH_GEQ
            | Some('<') -> junk(); ARITH_LEQ 
            | _ -> TERM_UNIFY end
        | '\\' -> if Some('=') = peek() then begin junk(); match peek() with
            | Some '=' -> junk(); TERM_INEQ
            | Some ':' -> junk(); if Some('=') = peek() then ARITH_INEQ else error "invalid token 3"
            | _ -> TERM_NOT_UNIFY end
        else error "invalid token 2"
        | _ -> EOF
    end
  with Stream.Failure -> failwith "TO DO"
