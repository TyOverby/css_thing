type identifier_kind =
  | Class of string
  | Id of string
[@@deriving sexp]

val extract_identifiers : string -> identifier_kind list
val rewrite_identifiers : string -> f:(identifier_kind -> string) -> string
