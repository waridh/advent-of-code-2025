open OUnit2

open Day_two

let tests = "test boundary invalid matcher" >::: [
    "test basic boundary" >:: (fun _ -> Invalid_detector.check_boundary "11" |> assert_equal true )
  ]

let () = run_test_tt_main tests
