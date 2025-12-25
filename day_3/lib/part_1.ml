let max3 a b c = a |> max b |> max c

let rec find_largest_pair_aux first second rest =
  match rest with
  | [] -> (first, second)
  | t :: [] -> (first, max t second)
  | [ h; t ] -> if h > first then (h, t) else (first, max3 second h t)
  | h :: t :: r ->
      if h > first then t :: r |> find_largest_pair_aux h t
      else t :: r |> find_largest_pair_aux first (max3 second h t)

let find_largest_pair l = find_largest_pair_aux 0 0 l

let find_largest_pair_of_string s =
  s |> Battery_row.row_list_of_row_string |> find_largest_pair
