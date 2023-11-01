open OUnit2
open Hw1

let test_number_in_month test_ctxt =
  assert_equal 2 (number_in_month ([(1, 1, 2022); (15, 2, 2022); (10, 1, 2022)], 1));
  assert_equal 1 (number_in_month ([(15, 2, 2022); (5, 5, 2022); (10, 10, 2022)], 2));
  assert_equal 1 (number_in_month ([(12, 6, 2022); (9, 9, 2022); (1, 3, 2022)], 3));
  assert_equal 1 (number_in_month ([(25, 12, 2022); (8, 8, 2022); (30, 4, 2022)], 4));
  assert_equal 0 (number_in_month ([(15, 6, 2022); (18, 6, 2022); (22, 6, 2022)], 5));
  assert_equal 0 (number_in_month ([], 10));
  assert_equal 0 (number_in_month ([(1, 1, 2022); (15, 2, 2022); (10, 1, 2022)], 12));
  assert_equal 3 (number_in_month ([(15, 6, 2022); (18, 6, 2022); (22, 6, 2022)], 6));
  assert_equal 1 (number_in_month ([(5, 5, 2022)], 5));  (* Single date with month 5 *)
  assert_equal 0 (number_in_month ([(1, 1, 2022)], 2)) (* Single date with month 1, not 2 *)

let test_is_older test_ctxt =
  assert_equal true (is_older ((1, 1, 2021), (15, 2, 2021)));
  assert_equal false (is_older ((15, 2, 2021), (1, 1, 2021)));
  assert_equal false (is_older ((1, 1, 2021), (1, 1, 2021)));
  assert_equal false (is_older ((20, 2, 2022), (15, 2, 2022)));
  assert_equal true (is_older ((15, 2, 2022), (20, 2, 2022)));
  assert_equal true (is_older ((1, 12, 2022), (1, 1, 2023)));
  assert_equal false (is_older ((1, 1, 2023), (1, 12, 2022)))

let test_number_in_months test_ctxt =
  assert_equal 3 (number_in_months ([(1, 1, 2022); (15, 2, 2022); (10, 10, 2022); (3, 6, 2023)], [1; 2; 6]));
  assert_equal 2 (number_in_months ([(15, 2, 2022); (5, 5, 2022); (10, 10, 2022)], [2; 5]));
  assert_equal 2 (number_in_months ([(12, 6, 2022); (9, 9, 2022); (1, 3, 2022)], [3; 4; 6]));
  assert_equal 2 (number_in_months ([(25, 12, 2022); (8, 8, 2022); (30, 4, 2022)], [4; 8]));
  assert_equal 0 (number_in_months ([(15, 6, 2022); (18, 6, 2022); (22, 6, 2022)], [5; 7]));
  assert_equal 0 (number_in_months ([], [1; 2; 3]));
  assert_equal 0 (number_in_months ([(1, 1, 2022); (15, 2, 2022); (10, 1, 2022)], [12]));
  assert_equal 3 (number_in_months ([(15, 6, 2022); (18, 6, 2022); (22, 6, 2022)], [6; 7]));
  assert_equal 1 (number_in_months ([(5, 5, 2022)], [5]));  (* Single date with month 5 *)
  assert_equal 0 (number_in_months ([(1, 1, 2022)], [2])) (* Single date with month 1, not 2 *)

let test_dates_in_month test_ctxt =
  assert_equal [(1, 1, 2022); (10, 1, 2022)] (dates_in_month ([(1, 1, 2022); (15, 2, 2022); (10, 1, 2022)], 1));
  assert_equal [(15, 2, 2022)] (dates_in_month ([(15, 2, 2022); (5, 5, 2022); (10, 10, 2022)], 2));
  assert_equal [(1, 3, 2022)] (dates_in_month ([(12, 6, 2022); (9, 9, 2022); (1, 3, 2022)], 3));
  assert_equal [(25, 12, 2022)] (dates_in_month ([(25, 12, 2022); (8, 8, 2022); (30, 4, 2022)], 12));
  assert_equal [(15, 6, 2022); (18, 6, 2022); (22, 6, 2022)] (dates_in_month ([(15, 6, 2022); (18, 6, 2022); (22, 6, 2022)], 6));
  assert_equal [] (dates_in_month ([], 5));
  assert_equal [] (dates_in_month ([(1, 1, 2022); (15, 2, 2022); (10, 1, 2022)], 4));
  assert_equal [(5, 5, 2022)] (dates_in_month ([(5, 5, 2022)], 5));  (* Single date with month 5 *)
  assert_equal [] (dates_in_month ([(1, 1, 2022)], 2))  (* Single date with month 1, not 2 *)

(* Test cases *)
let test_dates_in_months test_ctxt =
  let dates = [(2023, 1, 5); (2023, 2, 10); (2023, 3, 15); (2023, 4, 20); (2023, 5, 25)] in
  let months = [2; 4; 6] in
  assert_equal [(2023, 2, 10); (2023, 4, 20)] (dates_in_months (dates, months));

  let empty_dates = [] in
  let months = [2; 4; 6] in
  assert_equal [] (dates_in_months (empty_dates, months));

  let dates = [(2023, 1, 5); (2023, 2, 10); (2023, 3, 15); (2023, 4, 20); (2023, 5, 25)] in
  let months = [1; 3; 5] in
  assert_equal [(2023, 1, 5); (2023, 3, 15); (2023, 5, 25)] (dates_in_months (dates, months))

let test_get_nth test_ctxt =
  let strings = ["apple"; "banana"; "cherry"; "date"; "fig"] in
  
  assert_equal "apple" (get_nth strings 1);
  assert_equal "banana" (get_nth strings 2);
  assert_equal "cherry" (get_nth strings 3);
  assert_equal "date" (get_nth strings 4);
  assert_equal "fig" (get_nth strings 5)

(* Test cases *)
let test_string_of_date test_ctxt =
  assert_equal "September-10-2015" (string_of_date (10, 9, 2015));
  assert_equal "January-1-2000" (string_of_date (1, 1, 2000));
  assert_equal "December-25-2022" (string_of_date (25, 12, 2022))

let test_number_before_reaching_sum test_ctxt = 
  assert_equal 0 (number_before_reaching_sum [] 10);
  assert_equal 1 (number_before_reaching_sum [1] 2);
  assert_equal 0 (number_before_reaching_sum [5] 2);
  assert_equal 3 (number_before_reaching_sum [1; 2; 3; 4] 7);
  assert_equal 2 (number_before_reaching_sum [1; 2; 3; 4] 6)

let test_what_month test_ctxt = 
  assert_equal 1 (what_month 15);
  assert_equal 1 (what_month 31);
  assert_equal 2 (what_month 45);
  assert_equal 3 (what_month 60)

let test_month_range test_ctxt = 
  assert_equal [] (month_range 2 1);
  assert_equal [1; 2; 2; 2; 2] (month_range 31 35);
  assert_equal [1; 1; 1] (month_range 27 29)

let test_oldest test_ctxt = 
  assert_equal (Some (1, 1, 2000)) (oldest [(5, 1, 2000); (2, 3, 2000); (1, 1, 2000)]);
  assert_equal None (oldest [])

let test_cumulative_sum test_ctxt = 
  assert_equal [1; 3; 6; 10; 15] (cumulative_sum [1; 2; 3; 4; 5]);
  assert_equal [] (cumulative_sum []);
  assert_equal [1] (cumulative_sum [1])

let tests =
  "test_suite" >::: [
    "test_is_older" >:: test_is_older;
    "test_number_in_month" >:: test_number_in_month;
    "test_number_in_months" >:: test_number_in_months;
    "test_dates_in_month" >:: test_dates_in_month;
    "test_dates_in_months" >:: test_dates_in_months;
    "test_get_nth" >:: test_get_nth;
    "test_string_of_date" >:: test_string_of_date;
    "test_number_before_reaching_sum" >:: test_number_before_reaching_sum;
    "test_what_month" >:: test_what_month;
    "test_month_range" >:: test_month_range;
    "test_oldest" >:: test_oldest;
    "test_cumulative_sum" >:: test_cumulative_sum;
  ]

let () =
  run_test_tt_main tests