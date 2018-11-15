open Batteries
open Set.Infix
open RegExp

(* module type T = sig
   type t
   val empty: t
   val union: t -> t -> t
   val inter: t -> t -> t
   val singleton: 'a -> t
   val mem: 'a -> t -> bool
   val equal: t -> t -> bool
   val compare: t -> t -> int
   (* val full: int -> t *)
   val hash: t -> 'a
   val fold: ('b -> 'a -> 'a) -> t -> 'a -> 'a
   val shift: int -> t -> t
   val size: t -> int
   val rem: 'a -> t -> t
   val add: 'a -> t -> t
   val is_empty: t -> bool
   val intersect: t -> t -> bool
   val diff: t -> t -> t
   val subseteq: t -> t -> bool
   val set_compare: t -> t -> [`Lt|`Eq|`Gt|`N]
   val map: ('a -> 'a) -> t -> t
   val iter: ('a -> unit) -> t -> unit
   val filter: ('a -> bool) -> t -> t

   val forall: t -> ('a -> bool) -> bool
   val exists: t -> ('a -> bool) -> bool

   val to_list: t -> int list
   val of_list: int list -> t

   val print: out_channel -> t -> unit

   (* [random n p] returns a set whose elements are stricly 
     lesser than [n], and appear with a probability [p]  *)
   val random: int -> float -> t

   module Map: Hashtbl.S with type key = t
   end *)

(* module Mset = struct     
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
   end *)

type set = RegExp.regexp_t Set.t

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
      if x <=. y then None else
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
      if  x <=. y then true else
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
    open Set
    let empty = L Set.empty
    let set_compare x y =
      if x = y then `Eq else
      if x <=. y then `Lt
      else if y <=. x then `Gt
      else `N
    let rec xpass skipped t z = match t with
      | L x -> skipped, Set.union x z
      | N(x,tx,fx) ->
        if  x <=. z then
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
          if  x <=. y then L (Set.union y x')
          else N(x,L (Set.union y x'),t)
        | N(y,t,f) -> match set_compare x y with
          | `Eq -> N(y,add' t,f)
          | `Lt -> N(x,N(y,add' t,L x'),f)
          | `Gt -> N(y,add t,f)
          | `N  -> N(y,t,add f)
      in add
  end)

let check_final s1 s2 = 
  let r1 = Set.fold (fun x acc -> acc || if emptyWord x = Epsilon then true else false) s1 false in
  let r2 = Set.fold (fun x acc -> acc || if emptyWord x = Epsilon then true else false) s2 false in
  r1 = r2

let canonical alpha rUtd = true (*todo*)
(*  Set.fold (fun x acc -> S) rUtd Set.empty *)

let in_relation (x,y) r = canonical x r = canonical y r

let get_all_transitions a b sigma =
  let succ_a = Set.fold (fun c acc -> (c,(Set.fold (fun x acc' -> acc' ||. (derivate x (Set.singleton c))) a Set.empty)) :: acc) sigma [] in
  let succ_b = Set.fold (fun c acc -> (c,(Set.fold (fun x acc' -> acc' ||. (derivate x (Set.singleton c))) b Set.empty)) :: acc) sigma [] in
  let () = assert ((List.length succ_a) = (List.length succ_b)) in
  List.map2 (fun (c1,x) (c2,y) -> let () = assert (c1 = c2) in (x,y)) succ_a succ_b |> Set.of_list

let  hck s1 s2 sigma =
  let rec hck_aux r todo = 
    if Set.is_empty todo then true
    else let (x,y),td = RegExp.pop todo in
      if in_relation (x,y) r then hck_aux r td else
      if check_final x y then
        let td' = get_all_transitions x y sigma in
        hck_aux (Set.add (x,y) r) (td ||. td')
      else false 
  in hck_aux Set.empty (Set.singleton(s1,s2))
