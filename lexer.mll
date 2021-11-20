{
open Printf
open Lexing

exception Eof
exception TokenError of char
exception UnterminatedString of string

type token' =
  | NONE
  | INT of int
  | ID of string
  | STRCONST of string
  | PLUS
  | MINUS
  | ASTERISK
  | FSLASH
  | LPAREN
  | RPAREN
  | PERIOD
  | BEGIN
  | CONST
  | DO
  | END
  | IF
  | IN
  | ODD
  | OUT
  | PROCEDURE
  | THEN
  | VAR
  | WHILE
  | CALL
  | EQL
  | LT
  | GT
  | LEQ
  | GEQ
  | IS
  | COMMA
  | SEMICOLON
  | HASH
  | QUEST
  | EXCLAM

type state_str =
  | UNESCAPED
  | ESCAPED

(*
 * XXX Surely some package solves this variant type stringification thing?
 *)
let string_of_token = function
  | NONE        -> "NONE"
  | INT v       -> "Int: " ^ string_of_int v
  | ID v        -> "Identifier: " ^ v
  | STRCONST v  -> "String constant: '" ^ v ^ "'"
  | PLUS        -> "+"
  | MINUS       -> "-"
  | ASTERISK    -> "*"
  | FSLASH      -> "/"
  | LPAREN      -> "("
  | RPAREN      -> ")"
  | PERIOD      -> "."
  | BEGIN       -> "BEGIN"
  | CONST       -> "CONST"
  | DO          -> "DO"
  | END         -> "END"
  | IF          -> "IF"
  | IN          -> "IN"
  | ODD         -> "ODD"
  | OUT         -> "OUT"
  | PROCEDURE   -> "PROCEDURE"
  | THEN        -> "THEN"
  | VAR         -> "VAR"
  | WHILE       -> "WHILE"
  | CALL        -> "CALL"
  | EQL         -> "="
  | LT          -> "<"
  | GT          -> ">"
  | LEQ         -> "<="
  | GEQ         -> ">="
  | IS          -> ":="
  | COMMA       -> ","
  | SEMICOLON   -> ";"
  | HASH        -> "#"
  | QUEST       -> "?"
  | EXCLAM      -> "!"

class token (kind', lb) =
  object
    val _kind = kind'
    val _line = lb.Lexing.lex_curr_p.pos_lnum
    val _column = lb.Lexing.lex_curr_p.pos_cnum - lb.Lexing.lex_curr_p.pos_bol

    method kind = _kind
    method line = _line
    method column = _column
    method kindn = string_of_token _kind
  end

let t kind lb =
  new token (kind, lb)

}

(* XXX Handle strings with stateful are-we-escaped thing *)
rule tokenize = parse
  | [' ' '\t']      { tokenize lexbuf }
  | ['\n']          { new_line lexbuf;
                      tokenize lexbuf }
  | ['0' - '9']+ as v
                    { t (INT (int_of_string v)) lexbuf }
  | '\"'            { tokenize_string "" UNESCAPED lexbuf }
  | '+'             { t PLUS lexbuf }
  | '-'             { t MINUS lexbuf }
  | '*'             { t ASTERISK lexbuf }
  | '/'             { t FSLASH lexbuf }
  | '('             { t LPAREN lexbuf }
  | ')'             { t RPAREN lexbuf }
  | '.'             { t PERIOD lexbuf }
  | "BEGIN"         { t BEGIN lexbuf }
  | "CONST"         { t CONST lexbuf }
  | "DO"            { t DO lexbuf }
  | "END"           { t END lexbuf }
  | "IF"            { t IF lexbuf }
  | "IN"            { t IN lexbuf }
  | "ODD"           { t ODD lexbuf }
  | "OUT"           { t OUT lexbuf }
  | "PROCEDURE"     { t PROCEDURE lexbuf }
  | "THEN"          { t THEN lexbuf }
  | "VAR"           { t VAR lexbuf }
  | "WHILE"         { t WHILE lexbuf }
  | "CALL"          { t CALL lexbuf }
  | '='             { t EQL lexbuf }
  | '<'             { t LT lexbuf }
  | '>'             { t GT lexbuf }
  | "<="            { t LEQ lexbuf }
  | ">="            { t GEQ lexbuf }
  | ":="            { t IS lexbuf }
  | ';'             { t SEMICOLON lexbuf }
  | ','             { t COMMA lexbuf }
  | '#'             { t HASH lexbuf }
  | '?'             { t QUEST lexbuf }
  | '!'             { t EXCLAM lexbuf }
  | ['a'-'z' 'A' - 'Z']['a'-'z' 'A' - 'Z' '0' - '9']* as v
                    { t (ID v) lexbuf }
  | eof             { raise Eof }
  | _ as c          { raise (TokenError c) }

(*
 * XXX How to mark lexbuf (start, end) to avoid the concat?
 *     Also treat the escaped char concatenation properly.
 *)
and tokenize_string buf state = parse
  | '\n'            { new_line lexbuf;
                      tokenize_string buf state lexbuf }
  | "\""            { if state = UNESCAPED then
                        t (STRCONST buf) lexbuf
                      else
                        tokenize_string buf UNESCAPED lexbuf }
  | "\\"            { if state = UNESCAPED then
                        tokenize_string buf ESCAPED lexbuf
                      else
                        tokenize_string (buf ^ lexeme lexbuf)
                          UNESCAPED lexbuf }
  | eof             { raise (UnterminatedString buf) }
  | _               { if state = ESCAPED then
                        tokenize_string
                          (buf ^ "\\" ^ lexeme lexbuf)  UNESCAPED lexbuf
                      else
                        tokenize_string (buf ^ lexeme lexbuf)  state lexbuf
                    }
