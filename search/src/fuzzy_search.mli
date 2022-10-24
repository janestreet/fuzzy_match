open! Core

module Query : sig
  type t

  val create : string -> t
end

(** Fuzzy search scoring based on fzf
    - Give bonuses for characters at the start of words;
    - Give penalties for gaps;
    - The case of characters are ignored unless there is at least one uppercase character
      in the query. This is similar to 'smart case' behaviour in fzf except that fzf
      requires an exact case match when there is any uppercase character and we just apply
      a score deduction.
    - Queries are split into sub patterns delimited by
      spaces. All sub patterns must match the item and their scores are combined. This is
      the default behaviour in fzf called extended mode.
    - Scores are non-negative, lower scores are better, 0 means omit the result.

    {[
      # open Fuzzy_search;;
      # score (Query.create "foo") ~item:"bar"
      - : int = 0
    ]}

    {[
      # score (Query.create "abc") ~item:"bbb bbb bbb abc bbb"
      - : int = 7
    ]}

    {[
      # score (Query.create "abc") ~item:"zz   zabc   zz"
      - : int = 967
    ]}

    {[
      # score (Query.create "abc") ~item:"__a__b__c__"
      - : int = 1447
    ]}

    {[
      # score (Query.create "ghi abc") ~item:"abc def ghi jkl"
      - : int = 969
    ]}

    {[
      # score (Query.create "ghi pqr") ~item:"abc def ghi jkl"
      - : int = 0
    ]}
*)
val score : Query.t -> item:string -> int

(** Same as [score] but returns an option when the pattern doesn't match instead of 0. *)
val score_opt : Query.t -> item:string -> int option

(** If the query exists in the item returns the sorted and deduped indices of some fuzzy match.

    {[
      # matching_indices (Query.create "abc") ~item:"a__abb____c_c"
      - : int array option = Some [|3; 4; 10|]
    ]}

    {[
      # matching_indices (Query.create "no_match") ~item:"can't match this"
      - : int array option = None
    ]}

    {[
      # matching_indices (Query.create "abc ab") ~item:"a__abb____c_c"
      - : int array option = Some [|3; 4; 10|]
    ]}
*)
val matching_indices : Query.t -> item:string -> int array option

(** Splits an item into contiguous sections that reflect the fuzzy matching.

    Useful for highlighting queries in a UI.

    {[
      # split_by_matching_sections (Query.create "foo" ) ~item:"foo bar baz"
      - : ([ `Matching | `Not_matching ] * string) list option =
      Some [(`Matching, "foo"); (`Not_matching, " bar baz")]
    ]}

    {[
      # split_by_matching_sections (Query.create "abc" ) ~item:"____abc____"
      - : ([ `Matching | `Not_matching ] * string) list option =
      Some [(`Not_matching, "____"); (`Matching, "abc"); (`Not_matching, "____")]
    ]}

    {[
      # split_by_matching_sections  (Query.create "a ab" ) ~item:"a b"
      - : ([ `Matching | `Not_matching ] * string) list option =
      Some [(`Matching, "a"); (`Not_matching, " "); (`Matching, "b")]
    ]}
*)
val split_by_matching_sections
  :  Query.t
  -> item:string
  -> ([ `Matching | `Not_matching ] * string) list option

(** Sorts items by order of score. Items that don't match are omitted.

    Ties are broken by string length (smaller strings first), then alphabetical

    {[
      # let items = [ "___abc___"; "aBc"; "Abc_defgh"; "abc"; "foof" ];;
      val items : string list = ["___abc___"; "aBc"; "Abc_defgh"; "abc"; "foof"]
    ]}

    {[
      # search ~items (Query.create "abc");;
      - : string list = ["aBc"; "abc"; "Abc_defgh"; "___abc___"]
    ]}
*)
val search : Query.t -> items:string list -> string list

val search' : Query.t -> items:string array -> string array

module For_testing : sig
  (** When a match is found, returns [ Some (start, end) ] for word [ item[start..end-1] ]. *)
  val find_start_end_indices : query:string -> item:string -> unit -> (int * int) option
end
