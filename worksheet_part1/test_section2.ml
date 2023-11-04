open Section2

let date1 : int * int * int = (22, 2, 1900)
let date2 : date = (11, 1, 1900)

let test1A = dmy_to_mdy date1 = (2, 22, 1900)

let () = assert test1A

let test1B = dmy_to_mdy2 date2 = (1, 11, 1900)

let () = assert test1B

let list1 = ["hi"; "bye"]
let list2 = ["programming"; "languages"]
let list3 = [1; 2]
let list4 = [3; 4; 1]

let test2A = append list1 list2 = ["hi"; "bye"; "programming"; "languages"]

let () = assert test2A

let test2B = append list3 list4 = [1; 2; 3; 4; 1]

let () = assert test2B