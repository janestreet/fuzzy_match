open! Core

module Case_mode = struct
  type t = Smart
end

module Query = struct
  type t =
    { queries : string array (* The pattern split by spaces *)
    ; raw : string
    ; case_sensitive : bool
    }

  let create query =
    let queries =
      String.split query ~on:' '
      |> List.filter ~f:(fun s -> not (String.is_empty s))
      |> Array.of_list
    in
    let case_mode = Case_mode.Smart in
    let case_sensitive =
      match case_mode with
      | Smart -> String.exists query ~f:Char.is_uppercase
    in
    { queries; raw = query; case_sensitive }
  ;;

  let is_empty t = String.is_empty t.raw
end

module Start_end_indices = struct
  type t =
    { mutable found : bool
    ; mutable start_idx : int
    ; mutable end_idx : int
    }

  let the_one_and_only = { found = false; start_idx = 0; end_idx = 0 }
end

let equal_ignore_case char1 char2 =
  Char.equal (Char.lowercase char1) (Char.lowercase char2)
;;

let[@inline] find_start_end_indices ~query ~item () =
  let dst = Start_end_indices.the_one_and_only in
  let item_idx = ref 0 in
  let query_idx = ref 0 in
  while !item_idx < String.length item && !query_idx < String.length query do
    let item_char = String.get item !item_idx in
    let query_char = String.get query !query_idx in
    if equal_ignore_case item_char query_char then incr query_idx;
    incr item_idx
  done;
  if !query_idx < String.length query
  then dst.found <- false
  else (
    let end_idx = !item_idx in
    let item_idx = ref (end_idx - 1) in
    let query_idx = ref (String.length query - 1) in
    while !query_idx >= 0 do
      let item_char = String.get item !item_idx in
      let query_char = String.get query !query_idx in
      if equal_ignore_case item_char query_char then decr query_idx;
      decr item_idx
    done;
    let start_idx = !item_idx + 1 in
    dst.found <- true;
    dst.start_idx <- start_idx;
    dst.end_idx <- end_idx);
  dst
;;

let fold_matching_indices_single_query query ~item ~init ~f =
  let { Start_end_indices.start_idx; end_idx; found } =
    find_start_end_indices ~query ~item ()
  in
  let acc = ref init in
  if found
  then (
    let query_idx = ref 0 in
    for item_idx = start_idx to end_idx - 1 do
      let item_char = String.get item item_idx in
      let query_char = String.get query !query_idx in
      if equal_ignore_case item_char query_char
      then (
        acc := f !acc item_idx;
        incr query_idx)
    done);
  !acc
;;

let matching_indices (query : Query.t) ~item =
  if Query.is_empty query
  then Some [||]
  else (
    let indices =
      Array.fold query.queries ~init:Int.Set.empty ~f:(fun set query ->
        fold_matching_indices_single_query query ~item ~init:set ~f:Set.add)
    in
    if Set.is_empty indices then None else Some (Set.to_array indices))
;;

let split_by_matching_sections (query : Query.t) ~item =
  match matching_indices query ~item with
  | None -> None
  | Some [||] -> Some [ `Not_matching, item ]
  | Some matches ->
    let sections = Queue.create () in
    let add_section matching start end_inclusive =
      Queue.enqueue
        sections
        (matching, String.sub item ~pos:start ~len:(end_inclusive - start + 1))
    in
    let first = matches.(0) in
    if first > 0 then add_section `Not_matching 0 (first - 1);
    let matching_range_start = ref first in
    let matching_range_end = ref first in
    Array.iter matches ~f:(fun idx ->
      if idx > !matching_range_end + 1
      then (
        add_section `Matching !matching_range_start !matching_range_end;
        add_section `Not_matching (!matching_range_end + 1) (idx - 1);
        matching_range_start := idx);
      matching_range_end := idx);
    add_section `Matching !matching_range_start !matching_range_end;
    if !matching_range_end < String.length item - 1
    then add_section `Not_matching (!matching_range_end + 1) (String.length item - 1);
    Some (Queue.to_list sections)
;;

module Char_class = struct
  type t =
    | Upper
    | Lower
    | Digit
    | Non_word

  let of_char = function
    | 'A' .. 'Z' -> Upper
    | 'a' .. 'z' -> Lower
    | '0' .. '9' -> Digit
    | _ -> Non_word
  ;;
end

let start_of_item_bonus = 1
let start_of_word_bonus = 480
let camel_case_bonus = 360
let non_word_bonus = 480
let match_bonus = 320
let start_gap_penalty = 60
let continue_gap_penalty = 20

(* Wrong case penalty is only used when there is at least one uppercase character so we
   make it extreme *)
let wrong_case_penalty = 120
let first_char_multiplier = 2

let score_upper_bound ~query_length =
  (* Only characters that are part of the query can have positive score. The maximum
     positive score is when the first character of the query matches the first character
     of the item and it is also the start of a word. *)
  ((start_of_item_bonus + start_of_word_bonus) * first_char_multiplier * query_length) + 1
;;

(* For a [single_score_query] higher scores are better and we take score_upper_bound -
   score_single_query in score.

   Assumption:
   - [query] and [item] are not empty.
   - When there is no match, return 0. When the query is "" and item is not "" (infinitely
     many matches?), return score upper bound.
*)
let score_single_query query ~item ~case_sensitive =
  let { Start_end_indices.found; start_idx; end_idx } =
    find_start_end_indices ~query ~item ()
  in
  if not found
  then 0
  else (
    let start_char_class =
      if start_idx = 0
      then Char_class.Non_word
      else Char_class.of_char (String.get item (start_idx - 1))
    in
    let query_idx = ref 0 in
    let score = ref 0 in
    let in_gap = ref false in
    let prev_char_class = ref start_char_class in
    let prev_char_score = ref 0 in
    for item_idx = start_idx to end_idx - 1 do
      let item_char = String.get item item_idx in
      let query_char = String.get query !query_idx in
      if equal_ignore_case item_char query_char
      then (
        let char_class = Char_class.of_char item_char in
        let this_char_score =
          let base_score =
            match !prev_char_class, char_class with
            | Non_word, (Upper | Lower | Digit) -> start_of_word_bonus
            | Lower, Upper | _, Digit -> camel_case_bonus
            | _, Non_word -> non_word_bonus
            | _, _ -> match_bonus
          in
          let with_start_of_item_bonus =
            if item_idx = 0 then base_score + start_of_item_bonus else base_score
          in
          let with_consecutive_bonus =
            if !in_gap
            then with_start_of_item_bonus
            else Int.max with_start_of_item_bonus !prev_char_score
          in
          let with_wrong_case_penalty =
            if Char.equal item_char query_char || not case_sensitive
            then with_consecutive_bonus
            else with_consecutive_bonus - wrong_case_penalty
          in
          if !query_idx = 0
          then first_char_multiplier * with_wrong_case_penalty
          else with_wrong_case_penalty
        in
        incr query_idx;
        score := !score + this_char_score;
        prev_char_class := char_class;
        prev_char_score := this_char_score;
        in_gap := false)
      else (
        let this_char_score =
          if !in_gap then -continue_gap_penalty else -start_gap_penalty
        in
        score := !score + this_char_score;
        in_gap := true)
    done;
    !score)
;;

let score query ~item =
  match String.is_empty item, Query.is_empty query with
  | true, _ -> 0
  | _, true -> 1
  | _, _ ->
    let { Query.queries; raw; case_sensitive } = query in
    let raw_score = ref 0 in
    let any_mismatch = ref false in
    for i = 0 to Array.length queries - 1 do
      let query = queries.(i) in
      let score = score_single_query query ~item ~case_sensitive in
      if score = 0 then any_mismatch := true;
      raw_score := !raw_score + score
    done;
    (match !raw_score with
     | 0 -> 0
     | _ ->
       (* All subqueries must match some part of the item. *)
       if !any_mismatch
       then 0
       else score_upper_bound ~query_length:(String.length raw) - !raw_score)
;;

let score_opt query ~item =
  match score query ~item with
  | 0 -> None
  | x -> Some x
;;

module For_testing = struct
  let find_start_end_indices ~query ~item () =
    let { Start_end_indices.found; start_idx; end_idx } =
      find_start_end_indices ~query ~item ()
    in
    Option.some_if found (start_idx, end_idx)
  ;;
end

let search query ~items =
  List.filter_map items ~f:(fun item ->
    match score query ~item with
    | 0 -> None
    | score ->
      (* Tie break by shortest string first, then alphabetical. *)
      Some (score, String.length item, item))
  |> List.sort ~compare:[%compare: int * int * string]
  |> List.map ~f:Tuple3.get3
;;

let search' query ~items =
  let items_by_score =
    Array.filter_map items ~f:(fun item ->
      match score query ~item with
      | 0 -> None
      | score ->
        (* Tie break by shortest string first, then alphabetical. *)
        Some (score, String.length item, item))
  in
  Array.sort items_by_score ~compare:[%compare: int * int * string];
  Array.map items_by_score ~f:Tuple3.get3
;;
