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
    Error ("could not find - delimiter in: " ^ s)
  else
    match
      String.split_on_char '-' s |> List.map String.trim
      |> List.filter (fun x -> String.length x = 0 |> not)
    with
    | f :: s :: _ -> Ok (cons_id_range f s)
    | _ -> Error ("incorrect number of - found: " ^ s)

(** checks if input is even *)
let is_even x = x mod 2 = 0

let make_nines len = String.make len '9'
let make_lowest_nd_val n = "1" ^ String.make (n - 1) '0'

let cons_narrowed_range ll ul =
  cons_id_range (make_lowest_nd_val ll) (make_nines ul)

(** splits the range instance into a list of subranges that could have an
    invalid ID *)
let rec split_range r =
  let lower_r_l = lower_length r in
  let upper_r_l = upper_length r in
  if lower_r_l = upper_r_l then if is_even lower_r_l then [ r ] else []
  else if upper_r_l > lower_r_l then
    let upper_even = is_even upper_r_l in
    let lower_even = is_even lower_r_l in
    if upper_even && lower_even then
      (make_nines lower_r_l |> cons_id_range r.lower)
      :: (narrowed_split_range lower_r_l upper_r_l
         @ [ cons_id_range (make_lowest_nd_val upper_r_l) r.upper ])
    else if upper_even then
      narrowed_split_range lower_r_l upper_r_l
      @ [ cons_id_range (make_lowest_nd_val upper_r_l) r.upper ]
    else if lower_even then
      { lower = r.lower; upper = make_nines lower_r_l }
      :: narrowed_split_range lower_r_l upper_r_l
    else narrowed_split_range lower_r_l upper_r_l
  else []

and narrowed_split_range lower_length upper_length =
  split_range (cons_narrowed_range (lower_length + 1) (upper_length - 1))
