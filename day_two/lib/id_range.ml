type id_range = { lower : string; upper : string }
(** This record holds information on the range. The representation are for an
    inclusive range *)

(** Basic constructor for the id_range *)
let cons_id_range l u = { lower = l; upper = u }

(** function that retrieves number of digits in the lower range*)
let lower_length r = r.lower |> String.length

(** function that retrieves the number of digits in the upper range*)
let upper_length r = r.upper |> String.length

let int_of_lower r = int_of_string r.lower
let int_of_upper r = int_of_string r.upper

let in_range r v =
  let lower_r = int_of_lower r in
  let upper_r = int_of_upper r in
  v >= lower_r && v <= upper_r

(** Constructor for the id_range from the string representation *)
let id_range_of_string s =
  if String.contains s '-' |> not then
    Error ("could not fine - delimiter in: " ^ s)
  else
    match
      String.split_on_char '-' s |> List.map String.trim
      |> List.filter (fun x -> String.length x = 0 |> not)
    with
    | f :: s :: _ -> Ok (cons_id_range f s)
    | _ -> Error ("incorrect number of - found: " ^ s)

(** checks if input is even *)
let is_even x = x mod 2 = 0

(** splits the range instance into a list of subranges that could have an
    invalid ID *)
let split_range r =
  let lower_r_l = lower_length r in
  let upper_r_l = upper_length r in
  if lower_r_l = upper_r_l then if is_even lower_r_l then [ r ] else []
  else if upper_r_l > lower_r_l then
    [ { lower = r.lower; upper = String.make lower_r_l '9' } ]
  else []
