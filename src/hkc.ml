open Batteries
open Automata
open Graph
open RegExp
open Set.Infix

type set = regexp_t Set.t

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