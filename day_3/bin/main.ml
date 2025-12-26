open Day_3

let file = Sys.argv.(2)
let mode = Sys.argv.(1)
let sum_list l = List.fold_left ( + ) 0 l

let () =
  let ic = open_in file in
  try
    let lines = In_channel.input_lines ic in
    match mode with
    | "part-1" ->
        lines
        |> List.map Part_1.find_largest_pair_of_string
        |> List.map (fun (x, y) -> (x * 10) + y)
        |> sum_list |> print_int |> print_newline;
        close_in ic
    | "part-2" ->
        lines
        |> List.map Battery_row.row_list_of_row_string
        |> List.map (Part_2.make_n_largest_values 12)
        |> List.filter_map (fun x ->
            match x with
            | None -> None
            | Some y ->
                Some
                  (y |> List.map string_of_int
                  |> List.fold_left String.cat ""
                  |> int_of_string))
        |> sum_list |> print_int |> print_newline;
        close_in ic
    | _ ->
        print_endline
          ("invalid mode, " ^ mode ^ " please choose either part-1 or part-2");
        close_in ic
  with e ->
    close_in_noerr ic;
    raise e
