(** predicate that checks if the boundary value is an invalid id. *)
let check_boundary s =
  if String.length s mod 2 != 0 then false
  else String.ends_with ~suffix:(String.sub s 0 (String.length s / 2)) s

(** function that returns the boundary value if it is an invalid id, else return
    0*)
let get_invalid_boundary s = if check_boundary s then int_of_string s else 0

(** function that makes the *)
let invalid_id_of_half hs = hs ^ hs

let invalid_id_string_of_string_number s =
  s |> String_number.get_upper_half_digit |> invalid_id_of_half

let invalid_id_int_of_string_number s =
  s |> invalid_id_string_of_string_number |> int_of_string

(** function that will return the input value if it is within the range*)
let val_in_range v r = if Id_range.in_range r v then v else 0

(** predicate the checks if the provided range is a clean range*)
let clean_range (r : Id_range.id_range) =
  String_number.is_lower_zero r.lower && String_number.is_lower_nines r.upper

let broken_invariant (r : Id_range.id_range) =
  let lower = r.lower |> int_of_string in
  let upper = r.upper |> int_of_string in
  upper < lower

let clamp_down s =
  let s_msb = String_number.get_upper_half_digit s |> int_of_string in
  string_of_int (s_msb - 1)
  ^ String.make (String_number.lower_half_length s) '9'

let clamp_up s =
  let s_msb = String_number.get_upper_half_digit s |> int_of_string in
  string_of_int (s_msb + 1)
  ^ String.make (String_number.lower_half_length s) '0'

let rec get_clean_range_invalid_sum_aux acc curr upper =
  if curr > upper then acc
  else
    let curr_str = string_of_int curr in
    get_clean_range_invalid_sum_aux
      (acc + int_of_string (curr_str ^ curr_str))
      (curr + 1) upper

let get_clean_range_invalid_sum (r : Id_range.id_range) =
  let upper = r.upper |> String_number.int_of_upper_half_string_number in
  let lower = r.lower |> String_number.int_of_upper_half_string_number in
  get_clean_range_invalid_sum_aux 0 lower upper

let get_msb_upper (r : Id_range.id_range) =
  String_number.get_upper_half_digit r.upper

let get_msb_lower (r : Id_range.id_range) =
  String_number.get_upper_half_digit r.lower

let rec get_invalid_id_aux (r : Id_range.id_range) =
  if broken_invariant r then 0
  else if r.lower = r.upper then get_invalid_boundary r.lower
  else
    let msb_lower = get_msb_lower r in
    if msb_lower = get_msb_upper r then
      let msb_target = msb_lower ^ msb_lower in
      let target = msb_target |> int_of_string in
      if Id_range.in_range r target then msb_target |> int_of_string else 0
    else if clean_range r then get_clean_range_invalid_sum r
    else
      let clamped_lower = clamp_up r.lower in
      let clamped_upper = clamp_down r.upper in
      val_in_range
        (invalid_id_int_of_string_number r.lower)
        (Id_range.cons_id_range r.lower clamped_lower)
      + val_in_range
          (invalid_id_int_of_string_number r.upper)
          (Id_range.cons_id_range clamped_upper r.upper)
      + get_invalid_id_aux (Id_range.cons_id_range clamped_lower clamped_upper)

let list_map_sum fm l = l |> List.map fm |> List.fold_left ( + ) 0

let get_invalid_id r =
  r |> Id_range.split_range |> list_map_sum get_invalid_id_aux

let get_invalid_ids r = r |> list_map_sum get_invalid_id

(** function that determines the amount of invalid values from a single range *)
let rec check_range_aux (r : Id_range.id_range) =
  if broken_invariant r then 0
  else if r.lower = r.upper then 1
  else if clean_range r then
    let lower_msb =
      String_number.get_upper_half_digit r.lower |> int_of_string
    in
    let upper_msb =
      String_number.get_upper_half_digit r.upper |> int_of_string
    in
    upper_msb - lower_msb + 1
  else
    (check_boundary r.lower |> Bool.to_int)
    + (check_boundary r.upper |> Bool.to_int)
    + check_range_aux
        (Id_range.cons_id_range (clamp_up r.lower) (clamp_down r.upper))

(** function that will count the number of invalid values in a range *)
let check_range r =
  r |> Id_range.split_range |> List.map check_range_aux
  |> List.fold_left ( + ) 0

let check_ranges rs = rs |> List.map check_range |> List.fold_left ( + ) 0
