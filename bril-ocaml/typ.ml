type t =
  | Primitive of primitive
  | Parameterized of (string * t) list
[@@deriving show]

and primitive =
  | Integer of int
  | Boolean of bool
[@@deriving show]
