open Batteries
open Automata
open Graph
open RegExp
open Set.Infix
open Sets
(* type set = regexp_t Set.t *)

(*begin test *)


module Mset = struct     
  type t = regexp_t Set.t
  open Set
  let empty = empty
  let union = union
  let inter = intersect
  let singleton = singleton
  let mem = mem
  let equal = equal
  let compare = compare
  let fold = fold
  let size x = fold (fun _ i -> i + 1) x 0
  let rem = remove
  let add = add
  let is_empty = is_empty
  let subseteq = (<=.)
  (*TODO check this set_compare function *)
  let set_compare x y =
    if equal x y then `Eq else
    if Set.subset x y then `Lt
    else if Set.subset y x then `Gt
    else `N
  let map = map
  let iter = iter
  let hash = Hashtbl.hash
  let diff = diff
  let filter = filter
  let forall = for_all
  let exists = exists
  module Map = Hashtbl.Make(struct 
      type t = RegExp.regexp_t Set.t let equal = Set.equal let compare = compare let hash = hash 
    end)

end

type set = Mset.t



(* end test *)

module type QUEUE = sig
  type 'a t
  val empty: 'a t
  val push: 'a t -> 'a -> 'a t
  val pop: 'a t -> ('a * 'a t) option
  val filter: ('a -> bool) -> 'a t -> 'a t
  val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end 

let filter_rev f =
  let rec xfilter_rev acc = function
    | [] -> acc
    | x::q when f x -> xfilter_rev (x::acc) q
    | _::q -> xfilter_rev acc q
  in xfilter_rev []


module BFS = struct
  (* TODO: check Okasaki's book... *)
  type 'a t = 'a list * 'a list
  let empty = [],[]
  let push (h,k) x = h,x::k
  let pop (h,k) = match h with
    | x::h -> Some (x, (h,k))
    | [] -> match List.rev k with
      | [] -> None
      | x::h -> Some (x, (h,[]))
  let filter f =
    let rec xfilter last = function
      | [] -> last
      | x::q when f x -> x::xfilter last q
      | _::q -> xfilter last q
    in
    fun (h,k) -> xfilter (filter_rev f k) h,[]
  let fold f (h,k) a =  fold f h (fold f k a)
end

let rec fold f l a = match l with [] -> a | x::q -> fold f q (f x a)

module type CHECKER = sig
  (* how to enqueue elements of the todo list *)
  module Q: QUEUE
  (* checking outputs of the given sets *)
  val check: nfa -> set -> set -> bool
  (* unifying function (i.e., the up-to technique) *)
  val unify: unit -> set -> set -> (set * set) Q.t -> bool
end