open Day_two

let file = Sys.argv.(2)
let mode = Sys.argv.(1)

let () =
  let ic = open_in file in
  try
    let line = input_line ic in
    match mode with
    | "part-1" ->
        line |> String.split_on_char ','
        |> List.map Id_range.id_range_of_string
        |> List.filter_map (fun x ->
            match x with
            | Ok y -> Some y
            | Error _ -> begin
                print_endline "found error";
                None
              end)
        |> Invalid_detector.get_invalid_ids |> print_int |> print_newline;
        close_in ic
    | "part-2" ->
        line |> String.split_on_char ','
        |> List.map Id_range.id_range_of_string
        |> List.filter_map (fun x ->
            match x with Ok a -> Some a | Error _ -> None)
        |> Invalid_detector2.invalid_id_sum_of_ranges |> print_int
        |> print_newline
    | _ ->
        print_endline
          ("invalid mode, " ^ mode ^ " please choose either part-1 or part-2")
  with e ->
    close_in_noerr ic;
    raise e
