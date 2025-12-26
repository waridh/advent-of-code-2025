(** aux function that will find the furthest large value, and then return the
    value and the contents of the list before that value*)
let rec split_furthest_large_value_aux (value, lst) collected l =
  match l with
  | [] -> (value, lst)
  | h :: t ->
      if h >= value then
        split_furthest_large_value_aux
          (h, List.rev collected)
          (h :: collected) t
      else split_furthest_large_value_aux (value, lst) (h :: collected) t

(** function that takes a list, and returns a pair of the largest value furthest
    away from the start of the list, and a list of the values in between the
    start of the list and the matched value*)
let split_furthest_large_value l = split_furthest_large_value_aux (0, []) [] l

(** Invariant: The upper_list is the reverse form of the original list, and
    before the computation is complete, the acc will be reversed the final
    output*)
let rec make_n_largest_values_aux n acc upper_list lower_list =
  if n = 0 then Some (List.rev acc)
  else
    match lower_list with
    | [] -> None
    | lh :: lt ->
        let next_digit, new_upper =
          lh :: upper_list |> split_furthest_large_value
        in
        make_n_largest_values_aux (n - 1) (next_digit :: acc) new_upper lt

(** Function that returns a pair of list where there are n elements of the
    original list in the first of the pair, and the rest in the second of the
    pair. this implementation is missing tail call recursion, but we do not need
    it for our input size*)
let rec split_list n l =
  if n = 0 then ([], l)
  else
    match l with
    | [] -> ([], [])
    | h :: t ->
        let first, second = split_list (n - 1) t in
        (h :: first, second)

(** function that complete the core logic of the day 3 part 2 advent of code*)
let make_n_largest_values n l =
  let llength = List.length l in
  if llength < n then None
  else
    let upper_list, lower_list = split_list (llength - n) l in
    make_n_largest_values_aux n [] (List.rev upper_list) lower_list
