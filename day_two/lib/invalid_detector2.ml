(** The strategy for solving this part has now changed. *)

(** aux function that will see if s is made out of repeating pattern*)

let rec all l =
  match l with [] -> true | h :: t -> if not h then false else all t

let rec detect_match_aux pattern s =
  if pattern = "" then false
  else
    let curr_check =
      s |> Str.split (Str.regexp pattern) |> List.map (fun x -> x = "") |> all
    in
    if curr_check then true
    else detect_match_aux (String.sub pattern 0 (String.length pattern - 1)) s

(** predicate that checks if the input string matches the constraint for an
    invalid ID for part 2*)
let detect_match s = ()
