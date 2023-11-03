(* CSE 341, HW2 Provided Code *)

(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f
  
(* 1 *)
let make_silly_json i =
  let rec make_lst_helper n = 
    if n = 0 then []
    else (let b_field = ("b", True) in
    let n_field = ("n", Num (float_of_int n)) in
    Object [n_field; b_field]) :: make_lst_helper (n - 1)
  in Array (make_lst_helper i)

(* 2 *)
let rec concat_with (sep, ss) =
  match ss with
  | [] -> ""
  | hd :: tl -> let tl_concated = concat_with (sep, tl) in
  if tl_concated = "" then hd else hd ^ sep ^ tl_concated

(* 3 *)
let quote_string s =
  "\"" ^ s ^ "\""


(* 4 *)
let rec string_of_json j =
  match j with 
  | Num n -> json_string_of_float n
  | String s -> quote_string s
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array jlst -> let jslst = List.map string_of_json jlst in
    "[" ^ concat_with (", ", jslst) ^ "]"
  | Object fvlst -> let fvslst = List.map (fun (s, jv) -> quote_string s ^ " : " ^ string_of_json jv) fvlst in
    "{" ^ concat_with (", ", fvslst) ^ "}"

(* 5 *)
let rec take (n,xs) = 
  if n = 0 then []
  else match xs with
  | [] -> failwith "impossible case"
  | hd :: tl -> hd :: take (n-1, tl)

(* 6 *)
let rec firsts xs = 
  List.map (fun (f, s) -> f) xs

(* 7 *)
(* write your comment here *)
(* These two functions always evaluate to the same value, this can be shown by considering the case n = 0 and n >= 1*)
(* When n = 0 they both evaluate to empty list*)
(* When n >= 1, the first evaluates to the first component of the first n tuples in xs, 
   the latter evaluates to the first n of the first component of xs. These two values are obviously equivalent.*)

(* 8 *)
let rec assoc (k, xs) =
  match xs with
  | [] -> None
  | (k1, v1) :: tl -> if k = k1 then Some v1 else assoc (k, tl)

(* 9 *)
let dot (j, f) = 
  match j with
  | Object fvlst -> (let v_option = assoc (f, fvlst) in 
    match v_option with
    | None -> None
    | Some v -> Some v)
  | _ -> None

(* 10 *)
let rec dots (j, fs) =
  match fs with
  | [] -> Some j
  | hd :: tl -> (let v_option = dot (j, hd) in 
    match v_option with
    | None -> None
    | Some v -> dots (v, tl))

(* 11 *)
let one_fields j =
  let rec tail_helper acc fvlst = 
    match fvlst with
    | [] -> acc
    | (f, v) :: tl -> tail_helper (f :: acc) tl
  in match j with 
    | Object fvlst -> tail_helper [] fvlst
    | _ -> []

(* 12 *)
let no_repeats xs = 
  List.length (dedup xs) = List.length xs

(* 13 *)
let rec recursive_no_field_repeats j = 
  let rec extract_all_fields jv = 
    match jv with
    | Object fvlst -> let v_lst = List.map (fun (f, s) -> s) fvlst in 
      let sub_fields = List.map extract_all_fields v_lst in
        (firsts fvlst) @ (List.fold_left ( @ ) [] sub_fields)
    | Array jlst -> let sub_fields = List.map extract_all_fields jlst in List.fold_left ( @ ) [] sub_fields
    | _ -> []
  in let all_fields = extract_all_fields j in
  no_repeats all_fields

(* 14 *)
let count_occurrences (xs, e) =
  let rec tail_helper acc cur_str cur_cnt lst = 
    match lst with 
    | [] -> let acc = (cur_str, cur_cnt) :: acc in acc
    | hd :: tl -> if cur_str = hd then tail_helper acc cur_str (cur_cnt + 1) tl
    else if cur_str > hd then raise e
    else tail_helper (if cur_str = "" then acc else (cur_str, cur_cnt) :: acc) hd 1 tl
  in tail_helper [] "" 0 xs


(* 15 *)
let rec string_values_for_access_path (fs, js) = 
  match js with
  | [] -> []
  | hd :: tl -> let access_content = dots (hd, fs) in
    let str_values_tl = string_values_for_access_path (fs, tl) in
    match access_content with
    | None -> str_values_tl
    | Some j -> (match j with
                | String s -> s :: str_values_tl
                | _ -> str_values_tl)

(* 16 *)
let rec filter_access_path_value (fs, v, js) = 
  let filter_func j = 
    let access_content = dots (j, fs) in
      match access_content with
      | None -> false
      | Some jv -> (match jv with
                    | String s -> s = v
                    | _ -> false)
  in List.filter filter_func js

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r, p) = 
  p.latitude >= r.min_latitude && p.latitude <= r.max_latitude
  && p.longitude >= r.min_longitude && p.longitude <= r.max_longitude

(* 18 *)
let point_of_json j = 
  let latitude_value = dot (j, "latitude") in
  let longitude_value = dot (j, "longitude") in
  match (latitude_value, longitude_value) with
  | (Some j1, Some j2) -> (match (j1, j2) with
                          | (Num n1, Num n2) -> Some {latitude = n1; longitude = n2}
                          | _ -> None) 
  | _ -> None

(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) = 
  let filter_func j = 
    let access_content = dots (j, fs) in
    match access_content with
    | None -> false
    | Some jv -> let point_content = point_of_json jv in
      match point_content with
      | None -> false
      | Some p -> in_rect (r, p)
  in List.filter filter_func js

(* 20 *)
(* write your comment here *)
(* both can be rewritten using List.filter, you only need to write filter_func*)

(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. 
 * But notice our implementation of histogram uses *your* definition of count_occurrences
 *)
 (* We provide this code commented out because it uses some of your functions 
    that you haven't implemented yet *)


exception SortIsBroken

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude  =  47.648637;
    min_longitude = -122.322099;
    max_latitude  =  47.661176;
    max_longitude = -122.301019
  }

(* Creates a histogram for the given list of strings. 
 * Returns a tuple in which the first element is
 * a string, and the second is the number of times that string
 * is found. *)
let histogram xs = 
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences (sorted_xs, SortIsBroken) in
  let compare_counts (s1, n1) (s2, n2) =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs, js) = 
  histogram (string_values_for_access_path (fs,js))

(* notice we use *your* definition of dot *)
let complete_bus_positions_list =
  match (dot (complete_bus_positions, "entity")) with
  | Some (Array xs) -> xs
  | _ -> failwith "complete_bus_positions_list"


exception Unimplemented
let route_histogram     = histogram_for_access_path (["vehicle"; "trip"; "route_num"], complete_bus_positions_list)
let top_three_routes    = firsts (take (3, route_histogram))
let buses_in_ud         = filter_access_path_in_rect (["vehicle"; "position"], u_district, complete_bus_positions_list)
let ud_route_histogram  = histogram_for_access_path (["vehicle"; "trip"; "route_num"], buses_in_ud)
let top_three_ud_routes = firsts (take (3, ud_route_histogram))
let all_fourty_fours    = filter_access_path_value (["vehicle"; "trip"; "route_num"], "44", complete_bus_positions_list)
