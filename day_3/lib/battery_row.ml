(** converts a row string into an array of the corresponding number*)
let row_seq_of_row_string s =
  s |> String.to_seq |> Seq.map (fun x -> int_of_char x - 48)

let row_array_of_row_string s = s |> row_seq_of_row_string |> Array.of_seq
let row_list_of_row_string s = s |> row_seq_of_row_string |> List.of_seq
