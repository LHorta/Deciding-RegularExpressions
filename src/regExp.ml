open Batteries
open Set.Infix

type regexp_t =
  | Epsilon
  | Empty
  | Char of char
  | Concat of regexp_t * regexp_t
  | Choice of regexp_t * regexp_t
  | Sync of regexp_t * regexp_t
  | Star of regexp_t

(* Prints a string of the correspondent regular expression *)
let rec string_of_regexp s =
  match s with
    Empty         -> ""
  | Epsilon       -> "1"
  | Char c        ->  String.make 1 c
  | Choice (f,g)  ->  "("^(string_of_regexp f)^" + "^(string_of_regexp g)^")"
  | Concat (f,g)  ->  "("^(string_of_regexp f)^" . "^(string_of_regexp g)^")"
  | Sync (f,g)    ->  "("^(string_of_regexp f)^" : "^(string_of_regexp g)^")"
  | Star s      ->   (string_of_regexp s)^"*"

(**
    Some Operations over sets
 **)

(* TODO comment *)
let pop s =
  let x = Set.choose s in (x, Set.remove x s)

(* Prints a given set to a string *)
let sprint_set ~first ~last ~sep set =
  let rec print_set_aux s =
    if s <> Set.empty then
      let h,t = pop s in
      Printf.sprintf "%s%s" (String.make 1 h) (if t = Set.empty then "" else sep ^ " ") ^
      print_set_aux t
    else ""
  in Printf.sprintf "%s %s %s\n" first (print_set_aux set) last

(* Prints a given set *)
let print_set ~first ~last ~sep set =
  Printf.printf "%s " first;
  let rec print_set_aux s =
    if s <> Set.empty then
      let h,t = pop s in
      Printf.printf "%s%s" (string_of_regexp h) (if t = Set.empty then "" else sep ^ " ");
      print_set_aux t
  in print_set_aux set;
  Printf.printf " %s\n" last

(* creates a powerset of a given set *)
let rec powerset s =
  if s = Set.empty then Set.singleton Set.empty
  else let h,t = Set.pop s in Set.fold (fun t xs -> (xs <-- t) <-- (t <-- h)) (powerset t) (Set.empty)

(*circDot*)
let (<*>) a b =
  match a,b with
    Epsilon, Epsilon -> Set.singleton(Epsilon)
  | Epsilon, _ -> Set.singleton(b)
  | _, Epsilon -> Set.singleton(a)
  | _ -> Set.singleton(Concat(a,b))

(*circDotSet*)
let (<**>) a b =
  Set.fold (fun a' acc -> match a' with
        Empty -> acc
      | _ -> Set.fold (fun b' acc2 -> match b' with
            Empty -> acc2
          | _ -> Set.union (a' <*> b') acc2) b acc) a Set.empty;;

(*circProd*)
let (<:>) a b =
  match a,b with
  | Epsilon, Epsilon -> Set.singleton(Epsilon)
  | Epsilon, _ -> Set.singleton(b)
  | _, Epsilon -> Set.singleton(a)
  | _ -> Set.singleton(Sync(a,b))

(*circProdSet*)
let (<::>) a b =
  Set.fold (fun a' acc -> match a' with
        Empty -> acc
      | _ -> Set.fold (fun b' acc2 -> match b' with
            Empty -> acc2
          | _ -> Set.union (a' <:> b') acc2) b acc) a Set.empty;;

(**
   Operations over Regular Expressions
 **)


(* Prints a string of the correspondent regular expression *)
let rec format_regex = function
  | Empty -> "Empty"
  | Epsilon -> "Epsilon"
  | Char c -> "Char " ^ String.make 1 c
  | Concat(a, b)  -> "Concat("^format_regex a^", "^format_regex b^")"
  | Choice(a, b)  -> "Choice("^format_regex a^", "^format_regex b^")"
  | Sync(a,b)     -> "Sync("^format_regex a^", "^format_regex b^")"
  | Star a -> "Star("^format_regex a^")"

(* Function that checks for Empty Word Property *)
let rec emptyWord w =
  match w with
    Empty | Char _ -> Empty
  | Epsilon | Star _ -> Epsilon
  | Choice (f,g) -> (match (emptyWord f) with  Empty -> emptyWord g | Epsilon -> Epsilon | _ -> assert false)
  | Concat (f,g) | Sync(f,g) -> (match (emptyWord f) with Empty -> Empty    | Epsilon -> emptyWord g | _ -> assert false)

(* Function that returns a set of symbols in a given regular expression *)
let rec getSymbols exp =
  match exp with
    Empty | Epsilon-> Set.empty
  | Char c -> Set.singleton(c)
  | Choice (f,g) | Concat (f,g) | Sync (f,g) -> (getSymbols f) ||. (getSymbols g)
  | Star s -> getSymbols s

(* Function that creates a support set for a given regular expression *)
let rec piFun exp =
  match exp with
    Empty | Epsilon -> Set.empty
  | Char c -> Set.singleton(Epsilon)
  | Star s -> (piFun s) <**> (Set.singleton(Star s))
  | Choice(a,b) -> (piFun a) ||. (piFun b)
  | Concat(a,b) -> (piFun a) <**> Set.singleton(b) ||. (piFun b)
  | Sync(a,b) -> (piFun a) <::> (piFun b) ||. (piFun a) ||. (piFun b)

(* Function that return the set of all partial derivatives of a regular expression w.r.t x *)
let rec derivate exp (x : char Set.t) =
  match exp with
    Char a when Set.singleton a = x -> Set.singleton (Epsilon)
  | Empty  | Epsilon | Char _ -> Set.empty
  | Choice (f,g) -> (derivate f x) ||. (derivate g x)
  | Concat (f,g) -> ((derivate f x) <**> Set.singleton(g)) ||. (Set.singleton(emptyWord f) <**> (derivate g x))
  | Star s ->  (derivate s x) <**> Set.singleton(Star s)
  | Sync(f,g) when Set.cardinal x = 1 -> ((derivate f x) <::> (derivate g x)) ||. (Set.singleton(emptyWord f) <::> (derivate g x)) ||. (Set.singleton(emptyWord g) <::> (derivate f x))
  | Sync(f,g) -> let ps = ((powerset x) |> Set.remove Set.empty |> Set.remove x) in
    (Set.fold (fun c1 acc -> let c2 = x -. c1 in ((derivate f c1) <::> (derivate g c2)) ||. acc) ps Set.empty ) ||.
    (Set.singleton(emptyWord f) <::> (derivate g x)) ||. (Set.singleton(emptyWord g) <::> (derivate f x))
