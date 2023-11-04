let rec map f xs =
  match xs with
  | [] -> []
  | x :: xs' -> (f x) :: (map f xs')


let rec filter f xs =
  match xs with
  | [] -> []
  | x :: xs' -> if f x then x :: filter f xs' else filter f xs'


let rec fold_left f acc xs =
  match xs with
  | [] -> acc
  | x :: xs' -> fold_left f (f acc x) xs'

let sqrts_of_abs_all = map (fun f -> sqrt (Float.abs f))

let capitalize = map String.capitalize_ascii

let firsts = fun lst -> lst |> map (fun (x, _) -> x)

let no_empty_strings = filter (fun s -> s <> "")

let rec is_power_of_two i =
  i > 0 &&
    (i=1 || (i mod 2 = 0 && is_power_of_two (i / 2)))


let only_powers_of_two = filter is_power_of_two

let remove_negatives1 = filter (fun n -> n >= 0)

let remove_negatives2 = map remove_negatives1

let num_positive_float = fun lst -> lst |> map (fun f -> if f > 0.0 then 1 else 0) |> fold_left ( + ) 0

let reverse = fun lst -> lst |> fold_left (fun x y -> y :: x) []

let last xs default =
  let xs = reverse xs in 
  match xs with
  | [] -> default
  | hd :: tl -> hd


type funny_string_tree =
  | Empty
  | One of string
  | Node of funny_string_tree * funny_string_tree


let rec fold_left_tree f acc t =
  match t with
  | Empty -> acc
  | One s -> f acc s
  | Node (t1,t2) -> fold_left_tree f (fold_left_tree f acc t1) t2

let list_of_tree = fold_left_tree (fun x y -> y :: x) []

let has_string string = fold_left_tree (fun x y -> x || (y = string)) false

let rec exists pred tree = 
  match tree with
  | Empty -> false
  | One s -> pred s
  | Node (t1, t2) -> exists pred t1 || exists pred t2 

let has_string2 string = exists (fun s -> s = string)

let exists2 pred = fold_left_tree (fun x y -> x || pred y) false

let rec zipWith f alst blst = 
  match (alst, blst) with
  | ([], []) -> []
  | (hd1 :: tl1, []) -> []
  | ([], hd2 :: tl2) -> []
  | (hd1 :: tl1, hd2 :: tl2) -> (f hd1 hd2) :: (zipWith f tl1 tl2)

let first_bigger = fun lst -> lst |> zipWith (fun x y -> x > y)

let zipWith2 f alst blst= List.combine alst blst |> List.map f