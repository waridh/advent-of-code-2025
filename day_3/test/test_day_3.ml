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

let test_part_2_split_furthest =
  "function for finding the largest and deepest value, as well as returning \
   the rest of the list"
  >::: [
         ( "basic testing" >:: fun _ ->
           Part_2.split_furthest_large_value
             [ 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 ]
           |> assert_equal (9, [ 8; 1; 8; 1; 8; 1 ]) );
         ( "basic testing 2" >:: fun _ ->
           Part_2.split_furthest_large_value
             [ 1; 1; 1; 1; 1; 1; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
           |> assert_equal (9, [ 1; 1; 1; 1; 1; 1; 1; 2; 3; 4; 5; 6; 7; 8 ]) );
       ]

let test_part_2_split_list =
  "testing function that splits a list"
  >::: [
         ( "basic test" >:: fun _ ->
           Part_2.split_list 1 [ 1; 2; 3; 4; 5; 6 ]
           |> assert_equal ([ 1 ], [ 2; 3; 4; 5; 6 ]) );
         ( "test 2" >:: fun _ ->
           Part_2.split_list 0 [ 1; 2; 3; 4; 5; 6 ]
           |> assert_equal ([], [ 1; 2; 3; 4; 5; 6 ]) );
         ( "test 3" >:: fun _ ->
           Part_2.split_list 3 [ 1; 2; 3; 4; 5; 6 ]
           |> assert_equal ([ 1; 2; 3 ], [ 4; 5; 6 ]) );
         ( "test 4" >:: fun _ ->
           Part_2.split_list 6 [ 1; 2; 3; 4; 5; 6 ]
           |> assert_equal ([ 1; 2; 3; 4; 5; 6 ], []) );
         ( "test 5" >:: fun _ ->
           Part_2.split_list 8 [ 1; 2; 3; 4; 5; 6 ]
           |> assert_equal ([ 1; 2; 3; 4; 5; 6 ], []) );
       ]

let test_part_2_make_n_largest_values =
  "testing if we could run the core logic of part 2"
  >::: [
         ( "test 1" >:: fun _ ->
           Part_2.make_n_largest_values 12
             [ 1; 1; 1; 1; 1; 1; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
           |> assert_equal (Some [ 1; 1; 1; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]) );
         ( "test 2" >:: fun _ ->
           Part_2.make_n_largest_values 12
             [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1; 1; 1; 1 ]
           |> assert_equal (Some [ 9; 8; 7; 6; 5; 4; 3; 2; 1; 1; 1; 1 ]) );
         ( "test 3" >:: fun _ ->
           Part_2.make_n_largest_values 12
             [ 8; 1; 8; 1; 8; 1; 9; 1; 1; 1; 1; 2; 1; 1; 1 ]
           |> assert_equal (Some [ 8; 8; 8; 9; 1; 1; 1; 1; 2; 1; 1; 1 ]) );
       ]

let () =
  begin
    let _ =
      List.map run_test_tt_main
        [
          test_part_1_find_largest_pair;
          test_row_list_of_row_string;
          test_part_1_process_row;
          test_part_2_split_furthest;
          test_part_2_split_list;
          test_part_2_make_n_largest_values;
        ]
    in
    ()
  end
