(* args, instrs, name, type *)
type t =
  { name : string
  ; args : (string * string) list
  ; instrs : Instr.t list
  ; typ : string option
  }
[@@deriving show]
