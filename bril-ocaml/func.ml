(* args, instrs, name, type *)
type t =
  { name : string
  ; args : (string * Typ.t) list
  ; instrs : Instr.t list
  ; typ : Typ.t option
  }
[@@deriving show]
