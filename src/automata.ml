open Batteries
open RegExp
open Misc
open Set.Infix

(* representation of a vertex -- must be comparable *)
module Node = struct
  type t = regexp_t
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

(* representation of an edge -- must be comparable *)
module Edge = struct
  type t = string
  let compare = Pervasives.compare
  let equal = (=)
  let default = ""
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

(* more modules available, e.g. graph traversal with depth-first-search *)
module D = Graph.Traverse.Dfs(G)

module PathChecker = Graph.Path.Check(G)

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Dot(struct
    include G (* use the graph module from above *)
    let edge_attributes (a, e, b) = [`Label e; `Color 4711]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v =  if emptyWord v = Epsilon then  [`Shape `Doublecircle] else [`Shape `Ellipse]  (*`Label "\\N"*)
    let vertex_name v = "\"" ^ string_of_regexp v ^"\""
    let default_vertex_attributes _ = [`Shape `Ellipse]
    let graph_attributes _ = [`Rankdir `LeftToRight]
  end)


let rec getEdges exp src l g =
  if Set.is_empty exp then g
  else let e,s = pop exp in getEdges s src l (G.add_edge_e g (G.E.create src l e))

let buildAutomata exp =
  let g = G.empty in
  let sigma = (powerset (getSymbols exp)) |> Set.remove Set.empty in
  let states = Set.singleton(exp) ||. piFun exp in
  Set.fold (fun x acc ->
      Set.fold (fun y acc2 ->
          Set.fold (fun z acc3 -> G.add_vertex acc3 (G.V.create z) ) (*Create Nodes *)
            states (getEdges (derivate y x) y (sprint_set ~first:"{" ~sep:", " ~last:"}" x) acc2) ) (*Create Edges*)
        states acc)
    sigma g

(* let bAutomata exp g s =
  let sigma = (powerset (getSymbols exp)) *)


(* type for NFA *)
module NFA = struct
  include G
  type nft = {
    size: int;
    delta: G.t;
    (* delta.(a).(i) is the set of successors of state [i] along letter [a] *)
    accept: regexp_t Set.t;  }
  let size a = G.nb_vertex a.delta
  let accept a = fold_vertex (fun x acc -> if emptyWord x = Epsilon then Set.add x acc else acc) a.delta Set.empty
  let vars a = let r = fold_edges_e (fun x acc -> Set.add (G.E.label x) acc) a.delta Set.empty in Set.print ~first:"[" ~sep:"| " ~last:"]\n" print_any stdout r; r
    (* fold_edges_e Set.add a.delta Set.empty *)
  let delta a v x =
    try fold_succ_e (fun y acc -> if G.E.label y = v then Set.add (G.E.src y) acc else acc ) a.delta x Set.empty
    with Invalid_argument _ -> Set.empty
(* let delta a v x = fold_edges_e (fun y acc -> if (G.E.label y) = v && (G.E.src y) = x then Set.add (G.E.dst y) acc else acc) a.delta Set.empty *)
  let delta_set a v x = Set.fold (Set.union % delta a v) x Set.empty
end
type nfa = NFA.nft

open NFA
let from_graph (g: G.t) =
  let open G in
  let size = G.nb_vertex g in
  let accept = fold_vertex (fun x acc -> if emptyWord x = Epsilon then Set.add x acc else acc) g Set.empty in
  {size = size; delta = g; accept = accept}

open PathChecker
(* let live g (gs:G.V.t) =
  let pc = PathChecker.create g in
  G.fold_vertex (fun x acc -> if not (check_path pc x gs) then G.remove_vertex g x else g) g G.empty

(* normalised union: the resulting nfa only has live states, and no spurious letters *)
let nfa_normalised_union (g,s1,s2) =
  let g1 = live g s1 in
  let g2 = live g1 s2 in g2,from_graph g2 *)

let checkPath pc v initialStates =
  Set.fold (fun x acc -> acc || (check_path pc x v) ) initialStates false

(* let liveSet g (gs: G.V.t Set.t) =
   let pc = PathChecker.create g in
   G.fold_vertex (fun x acc -> if not (checkPath pc x gs) then G.remove_vertex g x else g) g G.empty *)

let liveSet g (gs: G.V.t Set.t) =
  (* let pc = PathChecker.create g in *)
  G.fold_vertex (fun x acc ->let pc = PathChecker.create acc in if not (checkPath pc x gs) then G.remove_vertex acc x else acc) g g

let nfa_normalised_union_set (g,s1,s2) =
  let g1 = liveSet g (Set.singleton s1 ||. Set.singleton s2) in g1, from_graph g1

let automata_union (g1,s1) (g2,s2) =
  (*let sigma1 = (powerset (getSymbols s1)) |> Set.remove Set.empty in
  let sigma2 = (powerset (getSymbols s2)) |> Set.remove Set.empty in*)
  fold_edges_e (fun x acc -> (G.add_edge_e acc x)) g2 (fold_edges_e (fun y acc2 -> (G.add_edge_e acc2 y)) g1 G.empty)
