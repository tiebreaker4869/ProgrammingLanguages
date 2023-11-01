(* CSE 341, Homework 1, Provided Code *)

(* You might choose to uncomment these, like the lecture code does *)
(* #utop_prompt_dummy
   let _ = UTop.set_show_box false *)

(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)

(**
 * TODO: Complete the 12 function bindings described in the assignment.  For the first (2), 
 * we have given you the correct first line and an incorrect function body.
 *)

 (* 1 *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) =
  let day1 = fst3 date1 in
  let month1 = snd3 date1 in
  let year1 = thd3 date1 in
  let day2 = fst3 date2 in
  let month2 = snd3 date2 in
  let year2 = thd3 date2 in
  if year1 <> year2 then year1 < year2 else 
    if month1 <> month2 then month1 < month2 else day1 < day2

(* 2 *)
let rec number_in_month ((dates : (int * int * int) list), (month : int)) =
  match dates with
  | [] -> 0
  | date :: rest -> number_in_month (rest, month) + (let m = snd3 date in if m = month then 1 else 0)

(* continue for 3 and onward here *)

(* 3 *)
let rec number_in_months ((dates: (int * int * int) list), (months: int list)) = 
  match months with
  | [] -> 0
  | month :: rest -> number_in_month (dates, month) + number_in_months (dates, rest) 

(*4 *)
let rec dates_in_month ((dates: (int * int * int) list), (month: int)) = 
  match dates with
  | [] -> []
  | date :: rest -> let m = snd3 date in 
  if m = month then date :: (dates_in_month (rest, month)) else dates_in_month (rest, month) 

(* 5 *)
let rec dates_in_months ((dates: (int * int * int) list), (months: int list)) = 
  match months with 
  | [] -> []
  | month :: rest -> dates_in_month (dates, month) @ dates_in_months (dates, rest)

(*6 *)
let rec get_nth (strs: string list) (n: int) = 
  if n = 1 then 
    match strs with 
    | [] -> "" (* impossible case *)
    | hd :: _ -> hd
 else 
    match strs with
    | [] -> "" (* impossible case *)
    | _ :: tl -> get_nth tl (n - 1)   

(*7 *)
let string_of_date date = 
  let name_of_months = ["January"; "February"; "March"; "April";
  "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"]
  in let (d, m, y) = date 
  in let y_str = string_of_int y
  in let m_str = get_nth name_of_months m
  in let d_str = string_of_int d
  in m_str ^ "-" ^ d_str ^ "-" ^ y_str   

(*8 *)
let rec number_before_reaching_sum (lst: int list) (sum: int) = 
  match lst with
  | [] -> 0
  | [x] -> if x < sum then 1 else 0
  | fst :: snd :: rest -> if fst < sum then 1 + number_before_reaching_sum (snd :: rest) (sum - fst)
  else 0

(*9 *)
let what_month day = 
  let month_days = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
  (number_before_reaching_sum month_days day) + 1

(*10 *)
let rec month_range day1 day2 =
  if day1 > day2 then [] else (what_month day1) :: (month_range (day1 + 1) day2) 

(*11 *)
let rec oldest dates =
  match dates with 
  | [] -> None
  | [x] -> Some x
  | hd :: snd :: tl -> let max_tl = oldest (snd :: tl) in
  match max_tl with
  | None -> max_tl (* impossible case*)
  | Some x -> if is_older (hd, x) then Some hd else max_tl 

(*12 *)
let cumulative_sum nums = 
  let rec sum_helper sum lst = 
    match lst with
    | [] -> []
    | hd :: tl -> let cur = hd + sum in cur :: (sum_helper cur tl) 
  in sum_helper 0 nums