open! Core

(** Determines whether a string matches (in the fuzziest sense) a pattern. [char_equal]
    can be used to choose between case-sensitivity and case-insensitivity. *)
val is_match : char_equal:(char -> char -> bool) -> pattern:string -> string -> bool
