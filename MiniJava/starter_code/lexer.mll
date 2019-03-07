{
  open Lexing
  open Parser

  exception Error of string

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let digit = ['0'-'9']
let integer = digit+
let space = [' ' '\t']
let letter = ['a'-'z''A'-'Z''_']
let ident = letter (digit | letter)*

rule get_token = parse
  | '\n'      { newline lexbuf; get_token lexbuf }
  | space+    { get_token lexbuf }
  | "true"    { BOOL_CONST true }
  | "false"   { BOOL_CONST false }
  | "int"     { INTEGER }
  | "boolean" { BOOLEAN }
  | "String"  { STRING }
  | "="       { ASSIGN }
  | "&&"      { AND }
  | "<"       { LT }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | ","       { COMMA }
  | "."       { DOT }
  | "!"       { NOT }
  | "length"  { LENGTH }
  | "class"   { CLASS }
  | "{"       { LBRACE }
  | "}"       { RBRACE }
  | "["       { LBRACKET }
  | "]"       { RBRACKET }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | ";"       { SEMICOLON }
  | "public"  { PUBLIC }
  | "static"  { STATIC }
  | "void"    { VOID }
  | "main"    { MAIN }
  | "System.out.println" { SYSO }
  | "return"  { RETURN }
  | "extends" { EXTENDS }
  | integer as i
      {
        try
          INT_CONST (Int32.of_string i)
        with Failure _ ->
          raise (Error "Invalid integer constant")
      }
  | ident as id { IDENT (Location.make (lexeme_start_p lexbuf) (lexeme_end_p lexbuf) id) }
  | eof     { EOF }
  | _ as c  { raise (Error ("Illegal character: " ^ String.make 1 c)) }
