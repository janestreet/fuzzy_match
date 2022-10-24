open! Core

let%expect_test _ =
  let test pattern text =
    print_endline
      (Bool.to_string (Fuzzy_match.is_match ~char_equal:Char.equal ~pattern text))
  in
  test "abc" "abc";
  [%expect {| true |}];
  test "abc" "adbecf";
  [%expect {| true |}];
  test "abc" "adbef";
  [%expect {| false |}];
  test "abc" "adbefc";
  [%expect {| true |}];
  test "abc" "dbefc";
  [%expect {| false |}];
  test "" "a";
  [%expect {| true |}];
  test "a" "";
  [%expect {| false |}];
  test "abc" "cba";
  [%expect {| false |}]
;;
