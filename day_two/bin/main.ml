open Day_two

let file = Sys.argv.(1)

(* let rec print_string_list (l : string list) = *)
(*   match l with *)
(*   | [] -> () *)
(*   | h :: t -> begin *)
(*       print_endline h; *)
(*       print_newline (); *)
(*       print_string_list t *)
(*     end *)

let () =
  let ic = open_in file in
  try
    let line = input_line ic in

    line |> String.split_on_char ','
    |> List.map Id_range.id_range_of_string
    |> List.filter_map (fun x ->
        match x with
        | Ok y -> Some y
        | Error _ -> begin
            print_endline "found error";
            None
          end)
    |> Invalid_detector.get_invalid_ids |> print_int;
    close_in ic
  with e ->
    close_in_noerr ic;
    raise e
