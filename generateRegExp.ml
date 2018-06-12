open Printf

(* type of regular expression *)
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

let nPairs, eqPairs, sigmaLength, outFile =  int_of_string Sys.argv.(1),int_of_string Sys.argv.(2),int_of_string Sys.argv.(3), Sys.argv.(4)

let () = 
  if (nPairs < 1 || eqPairs < 0) && eqPairs > nPairs then let _ = printf "ERROR! Usage: \"./generator nPairs numberOfEquivalentPairs numberOfCaractersInAlphabeth outputFile\".\nnPairs must be greater than 0\n" in exit 0
  else () 


let sigmaArray = Array.init 26 (fun x -> Char.chr (x+97))

let randomelement arr n =
    let i = Random.int n in Array.get arr i

(*let rec generate n s acc = 
    if n = 0 then acc 
    else *)