open Batteries
open Lexing
open RegExp
open List
open Automata
open Misc
open Set.Infix  (*toy*)

module HKC_BFS = Hck.Equivalence(Hck.BFS)


let hck = HKC_BFS.run

(*  "main loop" *)
let process name =
  let oc = if name="stdin" then stdin else open_in name in
  let lexbuf = Lexing.from_channel oc in
  let _ = init_pos lexbuf  name  in
  try
    let rl = Parser.main Lexer.token lexbuf in    
    let g1 = List.fold_left (fun acc x -> buildAutomata x acc) G.empty rl in
    (* FIXME test begin
    let tmpG1,tmp2 = pop g1 in
    let tmpG2,tmp3 = pop tmp2 in
    let g = automata_union (tmpG1,hd rl) (tmpG2, hd(tl rl)) in
    let b = true in let i = 7 in
    FIXME test end  *)
    let exp1,exp2 = (hd rl),(hd (tl rl)) in
    let sameSigma = Set.equal (getSymbols exp1) (getSymbols exp2)  in  
    (* begin toy 
    let _ = 
      Printf.printf "********************************\nexp1 --> ";
      print_set ~first:"{" ~sep:", " ~last:"}" (Set.singleton(exp1) ||. piFun exp1);
      Printf.printf "++++++++++++++++++++++++++++++++\nexp2 --> ";
      print_set ~first:"{" ~sep:", " ~last:"}" (Set.singleton(exp2) ||. piFun exp2);
      Printf.printf "********************************\n" in
    end toy *) 
    let g,nf = nfa_normalised_union_set (g1,hd rl, hd (tl rl)) in
    let b,i = if sameSigma then hck (Set.singleton(hd rl),Set.singleton(hd (tl rl) ),nf) else false,0 in
    let file = Legacy.open_out_bin "mygraph.dot" in let () = Dot.output_graph file g in
    Legacy.close_out file; print_endline(string_of_bool b ^ ": processed in " ^ string_of_int i ^ " iterations");
    List.iter (fun x -> Printf.printf "%s\n"(string_of_regexp x) ) rl; Printf.printf "*****************************************\n"
  with
  
  | LError msg -> print_lex_error msg
  | SError pos ->  print_syntax_error pos


(* invoca-se a função principal *)
let _ = process fname

(**
   let rec getEdges exp src l g =
   if Set.is_empty exp then g
   else let e,s = pop exp in getEdges s src l (G.add_edge_e g (G.E.create src l e))



   let buildAutomata exp g =
   let sigma = (powerset (getSymbols exp)) |> Set.remove Set.empty in
   let states = Set.singleton(exp) ||. piFun exp in
   (* let g = G.empty in *)
    Set.fold (fun x acc ->
      Set.fold (fun y acc2 ->
          Set.fold (fun z acc3 -> G.add_vertex acc3 (G.V.create z) ) (*Create Nodes *)
            states (getEdges (derivate y x) y (sprint_set ~first:"{" ~sep:", " ~last:"}" x) acc2) ) (*Create Edges*)
        states acc)
    sigma g

   let print_list ~first ~last ~sep lst =
   Printf.printf "%s " first;
   let rec print_list_aux s =
    if s <> [] then
      let h,t = hd s,tl s in
      Printf.printf "%c%s" h (if t = [] then "" else sep ^ " ");
      print_list_aux t
   in print_list_aux lst;
   Printf.printf " %s\n" last

   let hck = HKC_BFS.run

   let joinAutomata (g1,s1) (g2,s2) =
    let s3 = G.V.create Epsilon in
    let g3 =  G.add_vertex g1 s3 in
    let g4 = G.add_edge_e g3 (G.E.create s3 "" s1) in
    let g = G.add_edge_e g4 (G.E.create s3 "" s2)  in
    g

   let rec graphList gl rl =
   match gl with
    [] -> G.empty
   | h::t -> joinAutomata (h,(List.hd rl)) (List.hd t, (List.hd (List.tl rl)));;
 **)
