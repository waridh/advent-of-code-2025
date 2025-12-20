(* This record holds information on the range *)
type id_range = { lower : string; upper : string }

(* Basic constructor for the id_range *)
let cons_id_range l u = { lower = l; upper = u }
let lower_length r = r.lower |> String.length
let upper_length r = r.upper |> String.length

(* Constructor for the id_range from the string representation *)
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

(** splits the range instance into a list of subranges that could have an
    invalid ID *)
let split_range r =
  let lower_r_l = lower_length r in
  let upper_r_l = upper_length r in
  if lower_r_l = upper_r_l then [ r ]
  else if upper_r_l > lower_r_l then
    [ { lower = r.lower; upper = String.make lower_r_l '9' } ]
  else []
