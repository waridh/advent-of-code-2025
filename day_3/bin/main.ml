open Day_3

let file = Sys.argv.(2)
let mode = Sys.argv.(1)

let () =
  let ic = open_in file in
  try
    let lines = In_channel.input_lines ic in
    match mode with
    | "part-1" ->
        lines
        |> List.map Part_1.find_largest_pair_of_string
        |> List.map (fun (x, y) -> (x * 10) + y)
        |> List.fold_left ( + ) 0 |> print_int |> print_newline;
        close_in ic
    | "part-2" ->
        print_endline "part 2 is not implemented yet";
        close_in ic
    | _ ->
        print_endline
          ("invalid mode, " ^ mode ^ " please choose either part-1 or part-2");
        close_in ic
  with e ->
    close_in_noerr ic;
    raise e
