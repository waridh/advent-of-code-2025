(** The strategy for solving this part has now changed. *)

(** aux function that will see if s is made out of repeating pattern*)

let rec all l =
  match l with [] -> true | h :: t -> if not h then false else all t

let is_duplicate pattern s =
  s |> Str.split (Str.regexp pattern) |> List.map (fun x -> x = "") |> all

let rec detect_match_aux pattern s =
  if pattern = "" then false
  else
    let curr_check = is_duplicate pattern s in
    if curr_check then true
    else detect_match_aux (String.sub pattern 0 (String.length pattern - 1)) s

(** predicate that checks if the input string matches the constraint for an
    invalid ID for part 2*)
let detect_match s =
  s |> detect_match_aux (String_number.get_upper_half_digit s)

(** Function that combines the invalid IDs of a range *)
let invalid_id_of_range r =
  r
  |> Id_range.map_over_range string_of_int
  |> Seq.filter detect_match |> Seq.map int_of_string |> Seq.fold_left ( + ) 0

(** Function that finds all the invalid ID in all the ranges present in the
    input list, and then sum them up*)
let invalid_id_sum_of_ranges r =
  r |> List.map invalid_id_of_range |> List.fold_left ( + ) 0
