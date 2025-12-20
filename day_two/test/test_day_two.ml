open OUnit2
open Day_two

let tests_boundary_invalid_matcher =
  "test boundary invalid matcher"
  >::: [
         ( "test basic boundary" >:: fun _ ->
           Invalid_detector.check_boundary "11" |> assert_equal true );
         ( "test more complex condition" >:: fun _ ->
           Invalid_detector.check_boundary "123123" |> assert_equal true );
         ( "test false case" >:: fun _ ->
           Invalid_detector.check_boundary "101" |> assert_equal false );
       ]

let tests_id_range_parsing =
  "test the id_range type from string construction"
  >::: [
         ( "common case string parsing" >:: fun _ ->
           Id_range.id_range_of_string "123-456"
           |> assert_equal (Ok (Id_range.cons_id_range "123" "456")) );
         ( "missing - failure case" >:: fun _ ->
           Id_range.id_range_of_string "1234"
           |> Result.is_error |> assert_equal true );
         ( "missing second number case" >:: fun _ ->
           Id_range.id_range_of_string "1234-"
           |> Result.is_error |> assert_equal true );
         ( "secondary syntax check" >:: fun _ ->
           Id_range.id_range_of_string "1234----5678"
           |> assert_equal (Ok (Id_range.cons_id_range "1234" "5678")) );
       ]

let () =
  let tests = [ tests_boundary_invalid_matcher; tests_id_range_parsing ] in
  let _ = List.map run_test_tt_main tests in
  ()
