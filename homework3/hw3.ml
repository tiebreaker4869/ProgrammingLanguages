(* NOTE: Uncomment the following line if you want to #use this file in utop
 * (optionally, call it from utop directly): *)
(* #mod_use "hw3types.ml";; *)


(* NOTE: to get rid off the red-wiggles in VSCode, first compile the
 * the hw3types module running this 
 * from the command line: 
       ocamlopt hw3types.ml
 *)
 open Hw3types

 (**** Implement the following functions, remembering the "important note on function bindings" in the assignment write-up ****)
 
 (* #1 *)
 let only_lowercase =
   List.filter (fun s -> let c = s.[0] in let lower = Char.lowercase_ascii c in c = lower)
 
 (* #2 *)
 let longest_string1 =
  let retain_longer s1 s2 = let len1 = String.length s1 in let len2 = String.length s2 in 
    if len2 > len1 then s2 else s1
  in List.fold_left retain_longer "" 
 
 (* #3 *)
 let longest_string2 =
  let retain_longer s1 s2 = let len1 = String.length s1 in let len2 = String.length s2 in 
  if len2 >= len1 then s2 else s1
  in List.fold_left retain_longer "" 
 
 (* #4 *)
 let longest_string_helper f =
   let retain_winner s1 s2 = let len1 = String.length s1 in
   let len2 = String.length s2 in
   if f len2 len1 then s2 else s1
   in List.fold_left retain_winner ""
 
 let longest_string3 = longest_string_helper (>)
   
 
 let longest_string4 = longest_string_helper (>=)
   
 (* #5 *)
 let longest_lowercase =
   let filter = List.filter (fun s -> let c = s.[0] in let upper = Char.uppercase_ascii c in c <> upper) in
   longest_string1 % filter
 
 (* #6 *)
 let caps_no_X_string =
   String.fold_left (fun acc c -> if c = 'X' then acc else acc ^ String.make 1 c) "" % String.uppercase_ascii
 
 (* #7 *)
 let rec first_answer f xs = 
   match xs with 
   | [] -> raise NoAnswer
   | hd :: tl -> let v_option = f hd in
    match v_option with
    | None -> first_answer f tl
    | Some v -> v
 
 (* #8 *)
 let all_answers f xs = 
   match xs with
   | [] -> Some []
   | _ -> let transformed = List.map (fun x -> let lst = f x in match lst with | None -> [] | Some v -> v) xs in
   let folded = List.fold_left (fun acc x -> acc @ x) [] transformed in
   match folded with
   | [] -> None
   | _ -> Some folded 
 
 (* #9 *)
 let count_wildcards =
   g (fun () -> 1) (fun _ -> 0)
 
 let count_wild_and_variable_lengths =
   g (fun () -> 1) String.length
 
 let count_a_var s = 
   g (fun () -> 0) (fun x -> if x = s then 1 else 0)
 
 (* #10 *)
 let check_pat pat = 
  let rec gather_variable_names p = 
    match p with
    | VariableP n -> [n]
    | ConstructorP (_, p) -> gather_variable_names p
    | TupleP plst -> let ns = List.map gather_variable_names plst in
      List.fold_left (@) [] ns
    | _ -> []
  in let names = gather_variable_names pat in
  let uniq_names = List.sort_uniq (fun s1 s2 -> if s1 > s2 then 1 else if s1 = s2 then 0 else -1) names in
  List.length uniq_names = List.length names
 
 (* #11 *)
 let rec matches v pat = 
   match (v, pat) with
   | (_, WildcardP) -> Some []
   | (v, VariableP s) -> Some [(s, v)]
   | (Unit, UnitP) -> Some []
   | (Constant i, ConstantP ip) -> if i = ip then Some [] else None
   | (Constructor (c1, v), ConstructorP (c2, p)) -> if c1 = c2 then matches v p else None
   | (Tuple vlst, TupleP plst) -> let rec zip xs ys = (* assume xs and ys have same length*)
      match (xs, ys) with
      | ([], []) -> []
      | (hd1::tl1, hd2::tl2) -> (hd1, hd2) :: zip tl1 tl2
      | _ -> failwith "xs and ys should have same length"
    in let lenv = List.length vlst in
    let lenp = List.length plst in
    if lenv <> lenp then None else let vplst = zip vlst plst in
    let rec fold_helper lst acc1 acc2 =  (*acc1: match success or not, acc2: binding lst*)
      match lst with
      | [] -> (acc1, acc2)
      | hd :: tl -> let (vi, pi) = hd in 
        let hd_matched = matches vi pi in
        match hd_matched with
        | None -> fold_helper tl (acc1 && false) acc2
        | Some bindings -> fold_helper tl acc1 (acc2 @ bindings)
    in let (success, bindings) = fold_helper vplst true [] in
    if success then Some bindings else None
   | _ -> None
 
 (* #12 *)
 let first_match v pats = 
   try Some (first_answer (fun pat -> matches v pat) pats) with
   | NoAnswer -> None
 
 (* optional challenge problem  *)
(* unify to a narrower type*)
 let rec unify_type t1 t2 = 
  match (t1, t2) with
  | (_, AnythingT) -> t1
  | (AnythingT, _) -> t2
  | (UnitT, UnitT) -> UnitT
  | (IntT, IntT) -> IntT
  | (TupleT tlst1, TupleT tlst2) -> (
    try let unified_types = List.map2 unify_type tlst1 tlst2 in TupleT unified_types
    with Invalid_argument _ -> AnythingT  
  ) 
  | (VariantT v1, VariantT v2) -> if v1 = v2 then VariantT v1 else AnythingT
  | _ -> AnythingT
 
 let typecheck_patterns cons pats = 
   let n = List.length cons in
   let lookup_tbl = Hashtbl.create n in
   let rec build_lookup lst = 
    match lst with
    | [] -> ()
    | (c, v, _) :: tl -> Hashtbl.add lookup_tbl c v; build_lookup tl
   in let () = build_lookup cons in 
   let rec typecheck_pattern pat = 
    match pat with
    | WildcardP -> AnythingT
    | VariableP _ -> AnythingT
    | UnitP -> UnitT
    | ConstantP _ -> IntT
    | ConstructorP (s, _) -> (try VariantT (Hashtbl.find lookup_tbl s) with Not_found -> AnythingT)
    | TupleP plst -> let types_checked = List.map typecheck_pattern plst in
      TupleT types_checked
  in let types_checked = List.map typecheck_pattern pats in
  let result = List.fold_left unify_type AnythingT types_checked in
  match result with
  | AnythingT -> None
  | _ -> Some result