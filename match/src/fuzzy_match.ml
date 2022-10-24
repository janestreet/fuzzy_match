open! Core

let is_match ~char_equal ~pattern text =
  let pattern_length = String.length pattern in
  let text_length = String.length text in
  let rec helper pattern_index text_index =
    if pattern_index = pattern_length
    then true
    else if text_index = text_length
    then false
    else (
      let pattern_char = String.unsafe_get pattern pattern_index in
      let text_char = String.unsafe_get text text_index in
      if char_equal pattern_char text_char
      then helper (pattern_index + 1) (text_index + 1)
      else helper pattern_index (text_index + 1))
  in
  helper 0 0
;;
