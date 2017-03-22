open Lexing

(**
    parsing/lexing utilities
 **)

exception LError of string  (* lexing error - stores the character that originated the error *)

exception SError of Lexing.position (* parsing error - stores the position where the error ocurred *)

(* function that creates a string with a given position *)
let pp_position pos =
  ( "file :"^pos.pos_fname^
    ", line : "^string_of_int pos.pos_lnum ^
    ", column : "^string_of_int (pos.pos_cnum - pos.pos_bol))

(* lexing error function + exception throwing *)
let lex_error lexbuf st =
  let pos = (lexeme_start_p lexbuf) in (* posição do erro *)
  let msg =  ("Lexical Error in "^pp_position pos^"\n\tthe characters \""^st^"\" are not valid\n")  in
  raise (LError msg)

(* function that prints a lexical error message *)
let print_lex_error msg = Printf.fprintf stderr "%s\n%!" msg

(* function that prints a syntax error message *)
let print_syntax_error pos = Printf.fprintf stderr "Syntax Error in %s\n%!" (pp_position pos)

(* determins which file to handle: stdin or a file given as a paramenter in the command line *)
let fname = if (Array.length Sys.argv = 2) && (Sys.file_exists Sys.argv.(1)) then Sys.argv.(1) else "stdin"

(* function that resets postions in lx (of type lexbuf) based on the name of the known file f *)
let init_pos lx f =
  let pos = lx.lex_curr_p in
  lx.lex_curr_p <-
    {pos with pos_fname = f ; pos_lnum = 1 ; pos_bol = pos.pos_cnum}
