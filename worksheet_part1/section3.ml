let count_empty_string1 ss = 
  let rec tail_helper (acc, ss) = 
    match ss with
    | [] -> acc
    | s :: tl -> if s = "" then tail_helper (acc + 1, tl) else tail_helper (acc, tl)
  in tail_helper (0, ss)

let every_other1 xs = 
  let rec tail_helper acc xs = 
    match xs with
    | [] -> List.rev acc
    | x :: [] -> tail_helper (x :: acc) []
    | x :: _ :: tl -> tail_helper (x :: acc) tl
  in tail_helper [] xs

let rec map (f, xs) =
    match xs with
    | [] -> []
    | x :: xs' -> (f x) :: (map (f, xs'))
  
let rec filter (f, xs) =
    match xs with
    | [] -> []
    | x :: xs' -> if f x then x :: filter (f,xs') else filter (f,xs')

let sqrts_of_abs_all fs = 
  let f x = sqrt (Float.abs x) in map (f, fs)

let capitalize ss = map (String.capitalize_ascii, ss)

let firsts xs = map ((fun (f, s) -> f), xs)

let no_empty_strings ss = filter ((fun s -> s <> ""), ss)

let rec is_power_of_two i =
  i > 0 &&
    (i=1 || (i mod 2 = 0 && is_power_of_two (i / 2)))

let only_powers_of_two xs = filter (is_power_of_two, xs)

let remove_negatives1 xs = filter ((fun n -> n >= 0), xs)

let remove_negatives2 xss = map ((fun lst -> remove_negatives1 lst), xss)