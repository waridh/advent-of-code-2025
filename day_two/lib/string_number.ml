let half x = x / 2
let upper_half_length s = String.length s |> half

let lower_half_length s =
  let s_len = String.length s in
  s_len - half s_len

(** This function will obtain the upper sub-string of a string number *)
let get_upper_half_digit s = s |> String.length |> half |> String.sub s 0

(** This function will obtain the set difference of the original string and the
    return value from the get_upper_half_digit*)
let get_lower_half_digit s =
  let s_len = String.length s in
  s_len / 2 |> fun x -> String.sub s x (s_len - x)

let int_of_upper_half_string_number s =
  s |> get_upper_half_digit |> int_of_string

let is_lower_zero s = get_lower_half_digit s |> int_of_string |> fun x -> x = 0

let is_lower_nines s =
  let s_len = String.length s in
  get_lower_half_digit s = String.make (s_len - (s_len / 2)) '9'
