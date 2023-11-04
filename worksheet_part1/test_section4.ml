open Section4

let test1 = sqrts_of_abs_all [1.1; 2.2; 3.3; -4.4; -5.5] = [sqrt 1.1; sqrt 2.2; sqrt 3.3; sqrt 4.4; sqrt 5.5]

let () = assert test1

let test2 = capitalize ["abc"; "def"; "hhh"] = ["Abc"; "Def"; "Hhh"]

let () = assert test2

let test3 = firsts [(1, 2); (3, 4); (5, 6)] = [1; 3; 5]

let () = assert test3

let test4 = no_empty_strings [""; "aaa"; "bbb"; ""; ""] = ["aaa"; "bbb"]

let () = assert test4

let test5 = only_powers_of_two [1; 2; 3; 4; 5; 6; 7; 8] = [1; 2; 4; 8]

let () = assert test5

let test6 = remove_negatives1 [1; -2; 3; -4; 5] = [1; 3; 5]

let () = assert test6

let test7 = remove_negatives2 [[1; -1; 2; -2]; [3; -3; 4; -4]] = [[1; 2]; [3; 4]]

let () = assert test7

let test8 = num_positive_float [0.1; 0.2; -0.3; -0.4; 0.5] = 3

let () = assert test8

let test9 = reverse [1; 2; 3; 4; 5] = [5; 4; 3; 2; 1]

let () = assert test9

let test10 = last [1; 2; 3; 4; 5] 0 = 5

let () = assert test10

let test11 = last [] (-1) = (-1)

let () = assert test11

let tree1 = Node (Node (Node (One "abc", Empty), One "def"), Node (Node (One "aaa", One "bbb"), Empty))

let test12 = list_of_tree tree1 = List.rev ["abc"; "def"; "aaa"; "bbb"]

let () = assert test12

let test13 = has_string "abc" tree1 = true

let () = assert test13

let test14 = has_string "fff" tree1 = false

let () = assert test14

let test15 = exists (fun s -> s = "aaa") tree1

let () = assert test15

let test16 = exists (fun s -> String.length s > 4) tree1 = false

let () = assert test16

let test17 = has_string2 "abc" tree1 = true

let () = assert test17

let test18 = has_string2 "fff" tree1 = false

let () = assert test18

let test19 = exists2 (fun s -> s = "aaa") tree1

let () = assert test19

let test20 = exists2 (fun s -> String.length s > 4) tree1 = false

let () = assert test20

let lst1 = [1; 2; 3; 4; 5; 6]

let lst2 = [2; 3; 6; 7]

let test21 = zipWith (+) lst1 lst2 = [3; 5; 9; 11]

let () = assert test21

let test22 = first_bigger lst1 lst2 = [false; false; false]

let lst3 = [1; 2; 3; 4]

let lst4 = [2; 3; 4; 5]

let test23 = zipWith2 (fun (x, y) -> x + y) lst3 lst4 = [3; 5; 7; 9]

let () = assert test23