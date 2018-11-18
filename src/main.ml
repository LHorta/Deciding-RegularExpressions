open Batteries
open Lexing
open RegExp
open List
open Automata
open Misc
open Set.Infix  (*toy*)

(*module HKC_BFS = Hck.Equivalence(Hck.BFS)


  let hck = HKC_BFS.run*)

(*  "main loop" *)
let process name =
  let oc = if name="stdin" then stdin else open_in name in
  let lexbuf = Lexing.from_channel oc in
  let _ = init_pos lexbuf  name  in
  try
    let rl = Parser.main Lexer.token lexbuf in    
    let g = List.fold_left (fun acc x -> buildAutomata x acc) G.empty rl in
    let exp1,exp2 = (hd rl),(hd (tl rl)) in
    let s1,s2 = (getSymbols exp1), (getSymbols exp2) in
    let sameSigma = Set.equal s1 s2  in      
    let b = if sameSigma then Hkc.hck (Set.singleton (exp1)) (Set.singleton(exp2)) (s1) else false in
    let file = Legacy.open_out_bin "mygraph.dot" in let () = Dot.output_graph file g in
    Legacy.close_out file; print_endline(string_of_bool b) (*^ ": processed in " ^ string_of_int i ^ " iterations"*);
    List.iter (fun x -> Printf.printf "%s\n"(string_of_regexp x) ) rl; Printf.printf "*****************************************\n"
  with

  | LError msg -> print_lex_error msg
  | SError pos ->  print_syntax_error pos


(* invoca-se a função principal *)
let _ = process fname
