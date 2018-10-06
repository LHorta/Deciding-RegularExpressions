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

module type UPTO = sig
  (* the argument of create is the number of states *)
  type t
  val create: int -> t
  val unify: t -> set -> set -> bool
end

let filter_rev f =
  let rec xfilter_rev acc = function
    | [] -> acc
    | x::q when f x -> xfilter_rev (x::acc) q
    | _::q -> xfilter_rev acc q
  in xfilter_rev []

(* tail-rec [List.fold_left] with the type of [List.fold_right] *)
let rec fold f l a = match l with [] -> a | x::q -> fold f q (f x a)

(* FIFO queues give breadth-first traversal *)
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



(* LIFO queues give depth-first traversal *)
(*FIXME remove *)
module DFS = struct
  type 'a t = 'a list
  let empty = []
  let push r x = x :: r
  let rec fold f l a =  match l with [] -> a | x::q -> fold f q (f x a)
  let pop r = match r with
    | [] -> None
    | x::q -> Some (x,q)
  let filter = List.filter
  (* let fold = Common.fold *)
  let fold_vars vars f =
    let rec aux i x = if i = -1 then x else aux (i-1) (f i x) in
    aux (vars-1)
end

module type CHECKER = sig
  (* how to enqueue elements of the todo list *)
  module Q: QUEUE
  (* checking outputs of the given sets *)
  val check: nfa -> set -> set -> bool
  (* unifying function (i.e., the up-to technique) *)
  val unify: unit -> set -> set -> (set * set) Q.t -> bool
end

module Make(R: CHECKER): sig

  (* explore the nfa starting from the given sets, using the given checker *)
  val run: set * set * Automata.nfa -> bool * int
end = struct
  exception CE
  open NFA
  let run (x,y,t) =
    let tic = ref 0 in
    let vars = vars t in
    let push_span x y =
      incr tic;
      (* Set.fold (fun v todo -> R.Q.push todo (delta_set t (G.E.label v) x, delta_set t (G.E.label v) y)) vars *)
      Set.fold (fun v todo -> R.Q.push todo (delta_set t v x, delta_set t v y)) vars
    in
    let unify = R.unify() in
    (* begin debugging *)
    (*let print_todo s = R.Q.fold (fun v acc -> Set.singleton(v) ||. acc) s Set.empty |>
                       Set.iter (fun (x,y) -> 
                           Printf.printf "********************* TODO ***********************\n";
                           Printf.printf "A --> "; print_set ~first:"{" ~sep:", " ~last:"}" x; 
                           Printf.printf "B --> "; print_set ~first:"{" ~sep:", " ~last:"}" y;
                           Printf.printf "**********************END*************************\n")  in
    let print_pair (a,b) = Printf.printf "Pair -> "; (print_set ~first:"" ~sep:"," ~last:" --- " a); (print_set ~first:"" ~sep:"," ~last:"" b) in *)
    (*let print_pair (a,b) = Printf.printf "Pair -> {%s, %s}\n" (string_of_regexp a) (string_of_regexp b) in 
      end debugging *)
    let rec loop todo =     
      match R.Q.pop todo with
      | None -> true
      | Some ((x,y),todo) -> 
        (* debug 2 *)
        (*let () = print_pair (x,y);print_todo todo in*)
        (* end debug 2 *)
        if not (R.check t x y) then raise CE;
        if unify x y todo then loop todo
        else loop (push_span x y todo)
    in
    let r = try loop (R.Q.push R.Q.empty (x,y)) with CE -> false
    in r, !tic
end

module type RULES = sig
  (* representing a set of rewriting rules to perform efficient congruence tests.
     this module provides only the "one-pass" rewriting process *)
  type t
  val empty: t
  val add: set -> set -> t -> t	(* adding a rewriting rule to the candidate *)
  val pass: t -> set -> t * set	(* a single parallel rewriting pass *)
end

module type ERULES = sig
  (* more functions on rules, derived from the one-pass rewriting process *)
  type t
  val empty: t
  val add: set -> set -> t -> t
  val norm: t -> set -> set
  val pnorm: t -> set -> set -> (t*set) option
  val norm': ('a -> set -> 'a*set) -> t -> 'a -> set -> set
  val pnorm': ('a -> set -> 'a*set) -> t -> 'a -> set -> set -> (t*set) option
end

module ER(R: RULES): ERULES = struct
  include R

  (* get the normal form of [x] *)
  let rec norm rules x =
    let rules,x' = pass rules x in
    if Set.equal x x' then x else norm rules x'

  (* get the normal form of [y], unless [x] is subsumed by this normal
     form. In the first case, also return the subset of rules that
     were not applied *)
  let pnorm rules x y =
    let rec pnorm rules y =
      if Set.subset x y then None else
        let rules,y' = pass rules y in
        if Set.equal y y' then Some (rules,y') else pnorm rules y'
    in pnorm rules y

  (* get the normal form of [x] w.r.t a relation and a todo list *)
  let norm' f =
    let rec norm' rules todo x =
      let rules,x' = pass rules x in
      let todo,x'' = f todo x' in
      if Set.equal x x'' then x else norm' rules todo x''
    in norm'

  (* get the normal form of [y] w.r.t a relation, unless [x] is
     subsumed by the normal form of [y] w.r.t a relation and a todo
     list. In the first case, the normal form is only w.r.t the
     relation, and we also return the subset of (relation) rules that
     were not applied. *)
  let pnorm' f rules todo x y =
    let rec pnorm' rules todo y =
      if Set.subset x y then true else
        let todo,y' = f todo y in
        let rules,y'' = pass rules y' in
        if Set.equal y y'' then false else
          pnorm' rules todo y''
    in
    match pnorm rules x y with
    | None -> None
    | Some(rules,y') as r -> if pnorm' rules todo y' then None else r
end

module TR = ER(struct
    (* candidates as binary trees, allowing to cut some branches during
       rewriting *)
    type t = L of set | N of (set*t*t)
    let empty = L Set.empty
    let set_compare x y =
      if x = y then `Eq else
      if Set.subset x y then `Lt
      else if Set.subset y x then `Gt
      else `N
    let rec xpass skipped t z = match t with
      | L x -> skipped, Set.union x z
      | N(x,tx,fx) ->
        if  Set.subset x z then
          let skipped,z = xpass skipped tx z in
          xpass skipped fx z
        else
          let skipped,z = xpass skipped fx z in
          N(x,tx,skipped),z
    let pass = xpass empty
    let add x x' =
      let rec add' = function
        (* optimisable *)
        | L y -> L (Set.union y x')
        | N(y,t,f) -> N(y,t,add' f)
      in
      let rec add = function
        | L y as t ->
          if  Set.subset x y then L (Set.union y x')
          else N(x,L (Set.union y x'),t)
        | N(y,t,f) -> match set_compare x y with
          | `Eq -> N(y,add' t,f)
          | `Lt -> N(x,N(y,add' t,L x'),f)
          | `Gt -> N(y,add t,f)
          | `N  -> N(y,t,add f)
      in add
  end)

(* selecting the implementation of candidates *)
module R = TR

(* HKC algorithm for language equivalence *)
module Equivalence(Q: QUEUE) = Make(struct
    module Q = Q
    let check t x y = let open NFA in Set.intersect t.accept x = Set.intersect t.accept y
    let step todo z = Q.fold (fun (x,y as xy) (todo,z) ->
        if  Set.subset x z then todo, y ||. z else
        if Set.subset y z then todo, x ||. z else
          Q.push todo xy,z
      ) todo (Q.empty,z)
    let unify () =
      let r = ref R.empty in
      fun x y todo -> let r' = !r in
        match R.pnorm' step r' todo x y, R.pnorm' step r' todo y x with
        | None, None -> true
        | Some(ry,y'), None ->
          r := R.add y (R.norm ry (Set.union x y')) r'; false
        | None, Some(rx,x') ->
          r := R.add x (R.norm rx (Set.union x' y)) r'; false
        | Some(ry,y'), Some(_,x') ->
          let z = R.norm ry (Set.union x' y') in
          r := R.add x z (R.add y z r'); false
  end)
