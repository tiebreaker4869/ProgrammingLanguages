open Section3

let test1 = count_empty_string1 [""; "ss"; "ssa"; ""; ""] = 3

let () = assert test1

let test2 = every_other1 [1; 2; 3; 4; 5] = [1; 3; 5]

let () = assert test2

let test3 = sqrts_of_abs_all [1.1; 2.2; 3.3; -4.4; -5.5] = [sqrt 1.1; sqrt 2.2; sqrt 3.3; sqrt 4.4; sqrt 5.5]

let () = assert test3

let test4 = capitalize ["abc"; "def"; "hhh"] = ["Abc"; "Def"; "Hhh"]

let () = assert test4

let test5 = firsts [(1, 2); (3, 4); (5, 6)] = [1; 3; 5]

let () = assert test5

let test6 = no_empty_strings [""; "aaa"; "bbb"; ""; ""] = ["aaa"; "bbb"]

let () = assert test6

let test7 = only_powers_of_two [1; 2; 3; 4; 5; 6; 7; 8] = [1; 2; 4; 8]

let () = assert test7

let test8 = remove_negatives1 [1; -2; 3; -4; 5] = [1; 3; 5]

let () = assert test8

let test9 = remove_negatives2 [[1; -1; 2; -2]; [3; -3; 4; -4]] = [[1; 2]; [3; 4]]

let () = assert test9