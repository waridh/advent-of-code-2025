open OUnit2
open Day_3

let test_part_1_find_largest_pair =
  "testing if the auxilery searching function is correct"
  >::: [
         ( "simple match testing" >:: fun _ ->
           Part_1.find_largest_pair
             [ 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 ]
           |> assert_equal (9, 2) );
       ]

let test_row_list_of_row_string =
  "basic format transformation function"
  >::: [
         ( "basic" >:: fun _ ->
           Battery_row.row_list_of_row_string "818181911112111"
           |> assert_equal [ 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 ] );
       ]

let pair_printer (first, second) =
  "(" ^ string_of_int first ^ " " ^ string_of_int second ^ ")"

let test_part_1_process_row =
  "testing if we could find the largest pair in a string"
  >::: [
         ( "basic part 1 test" >:: fun _ ->
           Part_1.find_largest_pair_of_string "987654321111111"
           |> assert_equal (9, 8) ~printer:pair_printer );
         ( "basic part 1 2" >:: fun _ ->
           Part_1.find_largest_pair_of_string "811111111111119"
           |> assert_equal (8, 9) ~printer:pair_printer );
         ( "basic part 1 3" >:: fun _ ->
           Part_1.find_largest_pair_of_string "234234234234278"
           |> assert_equal (7, 8) ~printer:pair_printer );
         ( "basic part 1 4" >:: fun _ ->
           Part_1.find_largest_pair_of_string "818181911112111"
           |> assert_equal (9, 2) ~printer:pair_printer );
       ]

let () =
  begin
    let _ =
      List.map run_test_tt_main
        [
          test_part_1_find_largest_pair;
          test_row_list_of_row_string;
          test_part_1_process_row;
        ]
    in
    ()
  end
