
(* 1.A *)
let dmy_to_mdy (d, m, y) = (m, d, y)

type date = int * int * int

(* 1.B*)
let dmy_to_mdy2 (d: date) = let (d, m, y) = d in (m, d, y)

(* 2.A*)
let rec append xs ys = 
  match xs with
  | [] -> ys
  | hd :: tl -> hd :: (append tl ys)