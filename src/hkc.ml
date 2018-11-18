open Batteries
open Set.Infix
open RegExp

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

let to_string r =
  match r with 
  | Concat(Concat(Concat(Concat(Star(Char _), Char _), Star(Char _)), Char _), Star(Choice(Char _, Char _))) -> "alpha"
  | Concat(Concat(Concat(Concat(Star(Choice(Char _, Char _)), Char _), Star(Choice(Char _, Char _))), Char _), Star(Choice(Char _, Char _))) -> "beta"
  | Star(Choice(Char _, Char _)) -> "mu"
  | Concat(Concat(Star(Char _), Char _),Star(Choice(Char _, Char _))) -> "gamma"
  | Concat(Concat(Star(Choice(Char _, Char _)), Char _), Star(Choice(Char _, Char _))) -> "lambda"
  | _ -> ""

(* Prints a given set *)
let print_set_tmp ~first ~last ~sep set =
  Printf.printf "%s " first;
  let rec print_set_aux s =
    if s <> Set.empty then
      let h,t = pop s in
      Printf.printf "%s%s" (to_string h) (if t = Set.empty then "" else sep ^ " ");
      print_set_aux t
  in print_set_aux set;
  Printf.printf " %s" last

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
      if x <=. y then let () = print_endline("Saí - x <= y") in None else
        let rules,y' = pass rules y in
        if Set.equal y y' then let () = (print_set_tmp ~first:"{" ~sep:", " ~last:"}\n" y') in Some (rules,y') else pnorm rules y'
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
    let rec print_tree t = 
      match t with 
        L x -> print_set ~first:"L(" ~sep:", " ~last:")\n" x
      | N(x,l,r) -> Printf.printf "N( "; print_tree l; print_set ~first:"{" ~sep:", " ~last:"}" x; print_tree r
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

let step todo z = Set.fold (fun (x,y as xy) (todo,z) ->
    if  Set.subset x z then todo, Set.union y z else
    if Set.subset y z then todo, Set.union x z else
      (Set.add  xy todo), z
  ) todo (Set.empty,z)

let unify () =
  let r = ref TR.empty in
  fun x y todo -> let r' = !r in
    match TR.pnorm' step r' todo x y, TR.pnorm' step r' todo y x with
    | None, None -> true
    | Some(ry,y'), None ->
      r := TR.add y (TR.norm ry (Set.union x y')) r'; false
    | None, Some(rx,x') ->
      r := TR.add x (TR.norm rx (Set.union x' y)) r'; false
    | Some(ry,y'), Some(_,x') ->
      let z = TR.norm ry (Set.union x' y') in
      r := TR.add x z (TR.add y z r'); false

let in_relation (x,y) rUtd = unify () x y rUtd 

let get_all_transitions a b sigma =
  let succ_a = Set.fold (fun c acc -> (c,(Set.fold (fun x acc' -> acc' ||. (derivate x (Set.singleton c))) a Set.empty)) :: acc) sigma [] in
  let succ_b = Set.fold (fun c acc -> (c,(Set.fold (fun x acc' -> acc' ||. (derivate x (Set.singleton c))) b Set.empty)) :: acc) sigma [] in
  let () = assert ((List.length succ_a) = (List.length succ_b)) in
  List.map2 (fun (c1,x) (c2,y) -> let () = assert (c1 = c2) in (x,y)) succ_a succ_b |> Set.of_list

let  hck s1 s2 sigma =
  let rec hck_aux r todo = 
    if Set.is_empty todo then true
    else let (x,y),td = RegExp.pop todo in
      if in_relation (x,y) (r ||. td) then hck_aux r td else
      if check_final x y then
        let td' = get_all_transitions x y sigma in
        hck_aux (Set.add (x,y) r) (td ||. td')
      else false 
  in hck_aux Set.empty (Set.singleton(s1,s2))
