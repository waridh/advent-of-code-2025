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

let test_val_in_range =
  "tests if a value is in a range"
  >::: [
         ( "basic test" >:: fun _ ->
           Id_range.cons_id_range "101" "103"
           |> (fun r -> Id_range.in_range r 102)
           |> assert_equal true );
       ]

let run_split_test upper lower =
  Id_range.cons_id_range upper lower |> Id_range.split_range

let make_range_list l =
  List.map (fun (lower, upper) -> Id_range.cons_id_range lower upper) l

let tests_split_range =
  "test the split_range function."
  >::: [
         ( "testing base invalid case" >:: fun _ ->
           run_split_test "101" "102" |> assert_equal [] );
         ( "testing when the lower is even, and the upper is odd" >:: fun _ ->
           run_split_test "1111" "12345"
           |> assert_equal (make_range_list [ ("1111", "9999") ]) );
         ( "testing for when the lower range has odd length and the upper \
            range is even length"
         >:: fun _ ->
           run_split_test "111" "8888"
           |> assert_equal (make_range_list [ ("1000", "8888") ]) );
         ( "testing for even length upper range and lower range" >:: fun _ ->
           run_split_test "1111" "666666"
           |> assert_equal
                (make_range_list [ ("1111", "9999"); ("100000", "666666") ]) );
         ( "testing for greater range that should split" >:: fun _ ->
           run_split_test "111" "77777"
           |> assert_equal (make_range_list [ ("1000", "9999") ]) );
         ( "testing for when there is large enough range to have multple ranges"
         >:: fun _ ->
           run_split_test "111" "99999999"
           |> assert_equal
                (make_range_list
                   [
                     ("1000", "9999");
                     ("100000", "999999");
                     ("10000000", "99999999");
                   ]) );
         ( "testing if for large gap odd length lower and upper bound"
         >:: fun _ ->
           run_split_test "111" "7777777"
           |> assert_equal
                (make_range_list [ ("1000", "9999"); ("100000", "999999") ]) );
       ]

let tests_get_upper_sub_string =
  "test if we could get the upper half of a string number"
  >::: [
         ( "test basic" >:: fun _ ->
           String_number.get_upper_half_digit "111222" |> assert_equal "111" );
         ( "test odd length" >:: fun _ ->
           String_number.get_upper_half_digit "101" |> assert_equal "1" );
         ( "test odd length 2" >:: fun _ ->
           String_number.get_upper_half_digit "1234567" |> assert_equal "123" );
       ]

let tests_get_lower_sub_string =
  "testing if we could get the lower half of a string"
  >::: [
         ( "test basic" >:: fun _ ->
           String_number.get_lower_half_digit "111222" |> assert_equal "222" );
         ( "test odd length" >:: fun _ ->
           String_number.get_lower_half_digit "101" |> assert_equal "01" );
         ( "test odd length 2" >:: fun _ ->
           String_number.get_lower_half_digit "1234567" |> assert_equal "4567"
         );
       ]

let test_is_lower_zero =
  "checks the string predicate for lower half being zero"
  >::: [
         ( "test basic" >:: fun _ ->
           String_number.is_lower_zero "111000" |> assert_equal true );
         ( "test basic false" >:: fun _ ->
           String_number.is_lower_zero "111100" |> assert_equal false );
         ( "test false odd" >:: fun _ ->
           String_number.is_lower_zero "1111000" |> assert_equal false );
         ( "test true odd" >:: fun _ ->
           String_number.is_lower_zero "1110000" |> assert_equal true );
       ]

let test_is_lower_nines =
  "checks the string predicate for lower half being zero"
  >::: [
         ( "test basic" >:: fun _ ->
           String_number.is_lower_nines "111999" |> assert_equal true );
         ( "test basic false" >:: fun _ ->
           String_number.is_lower_nines "111100" |> assert_equal false );
         ( "test false odd" >:: fun _ ->
           String_number.is_lower_nines "1111999" |> assert_equal false );
         ( "test true odd" >:: fun _ ->
           String_number.is_lower_nines "1119999" |> assert_equal true );
       ]

let test_get_invalid_id_aux =
  "testing for the aux function for invalid id"
  >::: [
         ( "446443-446449" >:: fun _ ->
           Id_range.cons_id_range "446443" "446449"
           |> Invalid_detector.get_invalid_id_aux
           |> assert_equal 446446 ~printer:string_of_int );
       ]

let test_get_invalid_id =
  "testing if we could get the sum of invalid id from a range"
  >::: [
         ( "11-22" >:: fun _ ->
           Id_range.cons_id_range "11" "22"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 33 ~printer:string_of_int );
         ( "95-115" >:: fun _ ->
           Id_range.cons_id_range "95" "115"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 99 ~printer:string_of_int );
         ( "998-1012" >:: fun _ ->
           Id_range.cons_id_range "998" "1012"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 1010 ~printer:string_of_int );
         ( "1188511880-1188511890" >:: fun _ ->
           Id_range.cons_id_range "1188511880" "1188511890"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 1188511885 ~printer:string_of_int );
         ( "222220-222224" >:: fun _ ->
           Id_range.cons_id_range "222220" "222224"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 222222 ~printer:string_of_int );
         ( "1698522-1698528" >:: fun _ ->
           Id_range.cons_id_range "1698522" "1698528"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 0 ~printer:string_of_int );
         ( "446443-446449" >:: fun _ ->
           Id_range.cons_id_range "446443" "446449"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 446446 ~printer:string_of_int );
         ( "38593856-38593862" >:: fun _ ->
           Id_range.cons_id_range "38593856" "38593862"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 38593859 ~printer:string_of_int );
         ( "565653-565659" >:: fun _ ->
           Id_range.cons_id_range "565653" "565659"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 0 ~printer:string_of_int );
         ( "824824821-824824827" >:: fun _ ->
           Id_range.cons_id_range "824824821" "824824827"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 0 ~printer:string_of_int );
         ( "2121212118-2121212124" >:: fun _ ->
           Id_range.cons_id_range "2121212118" "2121212124"
           |> Invalid_detector.get_invalid_id
           |> assert_equal 0 ~printer:string_of_int );
       ]

let test_get_invalid_ids =
  "testing if we could convert a list of ranges to the number of invalid values"
  >::: [
         ( "testing the provided example" >:: fun _ ->
           make_range_list
             [
               ("11", "22");
               ("95", "115");
               ("998", "1012");
               ("1188511880", "1188511890");
               ("222220", "222224");
               ("1698522", "1698528");
               ("446443", "446449");
               ("38593856", "38593862");
               ("565653", "565659");
               ("824824821", "824824827");
               ("2121212118", "2121212124");
             ]
           |> Invalid_detector.get_invalid_ids
           |> assert_equal 1227775554 ~printer:string_of_int );
       ]

let () =
  let tests =
    [
      tests_boundary_invalid_matcher;
      tests_id_range_parsing;
      tests_split_range;
      test_val_in_range;
      tests_get_upper_sub_string;
      tests_get_lower_sub_string;
      test_is_lower_zero;
      test_is_lower_nines;
      test_get_invalid_id_aux;
      test_get_invalid_id;
      test_get_invalid_ids;
    ]
  in
  let _ = List.map run_test_tt_main tests in
  ()
