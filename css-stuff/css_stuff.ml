open! Core_kernel
open Css_lib.Types

type identifier_kind =
  | Class of string
  | Id of string
[@@deriving sexp]

let map_component_value ~f = function
  | Ident ident when String.is_prefix ident ~prefix:"." -> Ident (f (Class ident))
  | Hash hash -> Hash (f (Id hash))
  | other -> other
;;

let rec map_selector ~f = function
  | Simple list -> Simple (List.map list ~f:(map_component_value ~f))
  | Compound list -> Compound (List.map list ~f:(map_component_value ~f))
  | Complex list -> Complex (List.map list ~f:(map_selector ~f))
;;

let map_at_rule (at_rule : atrule) ~f =
  let prelude = at_rule.prelude in
  let prelude = List.map prelude ~f:(List.map ~f:(map_component_value ~f)) in
  { at_rule with prelude }
;;

let map_style_rule (rule : styleRule) ~f =
  let prelude = rule.prelude in
  let prelude = List.map prelude ~f:(map_selector ~f) in
  { rule with prelude }
;;

let map_rule ~f = function
  | StyleRule style_rule -> StyleRule (map_style_rule style_rule ~f)
  | AtRule at_rule -> AtRule (map_at_rule at_rule ~f)
  | other -> other
;;

let map_stylesheet stylesheet ~f = List.map stylesheet ~f:(map_rule ~f)

let extract_identifiers css_string =
  let stylesheet = Css_lib.Index.parse css_string in
  let idents = ref [] in
  let _ =
    map_stylesheet stylesheet ~f:(fun ident ->
        idents := ident :: !idents;
        match ident with
        | Class c -> c
        | Id h -> h)
  in
  List.rev !idents
;;

let rewrite_identifiers css_string ~f =
  let stylesheet = Css_lib.Index.parse css_string in
  let stylesheet = map_stylesheet stylesheet ~f in
  Css_lib.Index.print stylesheet
;;

let test s =
  let identifiers = extract_identifiers s in
  let rewritten =
    rewrite_identifiers s ~f:(function
        | Class s -> s ^ "-class-rewritten"
        | Id s -> s ^ "-id-rewritten")
  in
  print_s [%message (identifiers : identifier_kind list)];
  print_endline rewritten
;;

let%expect_test _ =
  test "a {}";
  [%expect "\n      (identifiers ())\n      a {\n\n      }"]
;;

let%expect_test _ =
  test ".a {}";
  [%expect
    "\n      (identifiers ((Class .a)))\n      .a-class-rewritten {\n\n      }"]
;;

let%expect_test _ =
  test ".a > .b {}";
  [%expect
    "\n\
    \      (identifiers ((Class .a) (Class .b)))\n\
    \      .a-class-rewritten>.b-class-rewritten {\n\n\
    \      }"]
;;

let%expect_test _ =
  test ".a .b {}";
  [%expect
    "\n\
    \      (identifiers ((Class .a) (Class .b)))\n\
    \      .a-class-rewritten .b-class-rewritten {\n\n\
    \      }"]
;;

let%expect_test _ =
  test ".a:hover {}";
  [%expect
    "\n      (identifiers ((Class .a)))\n      .a-class-rewritten:hover {\n\n      }"]
;;

let%expect_test _ =
  test ".a .b > .c {}";
  [%expect
    "\n\
    \      (identifiers ((Class .a) (Class .b) (Class .c)))\n\
    \      .a-class-rewritten .b-class-rewritten>.c-class-rewritten {\n\n\
    \      }"]
;;

let%expect_test _ =
  test "#a {}";
  [%expect "\n      (identifiers ((Id a)))\n      #a-id-rewritten {\n\n      }"]
;;

let%expect_test _ =
  test "#a::first-letter {}";
  [%expect
    "\n      (identifiers ((Id a)))\n      #a-id-rewritten::first-letter {\n\n      }"]
;;

let%expect_test _ =
  test {| #a[href^="http"] {} |};
  [%expect
    "\n\
    \      (identifiers ((Id a)))\n\
    \      #a-id-rewritten[href^=\"http\"] {\n\n\
    \      }"]
;;

let%expect_test _ =
  test "#a > #b {}";
  [%expect
    "\n\
    \      (identifiers ((Id a) (Id b)))\n\
    \      #a-id-rewritten>#b-id-rewritten {\n\n\
    \      }"]
;;

let%expect_test _ =
  test "#a #b {}";
  [%expect
    "\n\
    \      (identifiers ((Id a) (Id b)))\n\
    \      #a-id-rewritten #b-id-rewritten {\n\n\
    \      }"]
;;

let%expect_test _ =
  test "#a ~ #b {}";
  [%expect
    "\n\
    \      (identifiers ((Id a) (Id b)))\n\
    \      #a-id-rewritten~#b-id-rewritten {\n\n\
    \      }"]
;;

let%expect_test _ =
  test "#a:hover {}";
  [%expect "\n      (identifiers ((Id a)))\n      #a-id-rewritten:hover {\n\n      }"]
;;

let%expect_test _ =
  test "#a #b > #c {}";
  [%expect
    "\n\
    \      (identifiers ((Id a) (Id b) (Id c)))\n\
    \      #a-id-rewritten #b-id-rewritten>#c-id-rewritten {\n\n\
    \      }"]
;;
