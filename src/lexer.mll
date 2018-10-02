{
  open RegExp
  open Misc
  open Lexing
  open Parser
}


let space = [' ' '\t']

rule token = parse
  | space          { token lexbuf }
  | '\n'           { new_line lexbuf; EOL }
  | ['a'-'z'] as c { CHAR c }
  | '0'            { EMPTY }
  | '1'            { EPSILON } 
  | '*'            { STAR }   (* the kleene star operator *)
  | '+'            { CHOICE } (* the union operator *)
  | '.'            { CONCAT } (*the concatenation operator *)
  | ':'            { SYNC }   (* the synchronous operator *)
  | '('            { LPAR }
  | ')'            { RPAR }
  | eof            { EOF }
  | _ as st        { lex_error lexbuf (String.make 1 st)}
