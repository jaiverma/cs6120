module J = Yojson.Safe

type t =
  | Arithmetic of arithmetic
  | Comparison of comparison
  | Logic of logic
  | Control of control
  | Misc of misc
  | Label of string
[@@deriving show]

and arithmetic =
  | Add of (string * string)
  | Mul of (string * string)
  | Sub of (string * string)
  | Div of (string * string)
[@@deriving show]

and comparison =
  | Eq of (string * string)
  | Lt of (string * string)
  | Gt of (string * string)
  | Le of (string * string)
  | Ge of (string * string)
[@@deriving show]

and logic =
  | Not of bool
  | And of (bool * bool)
  | Or of (bool * bool)
[@@deriving show]

and control =
  | Jmp of string
  | Br of (bool * string * string)
  | Call of string
  | Ret
[@@deriving show]

and misc =
  | Id of Typ.t
  | Print of Typ.t list
  | Nop
  | Const of Typ.t
[@@deriving show]

let of_yojson json =
  match List.find_opt (fun key -> key = "label") @@ J.Util.keys json with
  | Some _ ->
    let label = J.Util.member "label" json |> J.Util.to_string in
    Label label
  | None ->
    let op = J.Util.member "op" json |> J.Util.to_string in
    (match op with
    | "const" ->
      let dest = J.Util.member "dest" json |> J.Util.to_string in
      let typ = J.Util.member "type" json |> J.Util.to_string in
      let instr =
        match typ with
        | "int" ->
          let value = J.Util.member "value" json |> J.Util.to_int in
          Misc (Const (Primitive (Integer value)))
        | "bool" ->
          let value = J.Util.member "value" json |> J.Util.to_bool in
          Misc (Const (Primitive (Boolean value)))
        | _ -> failwith @@ Printf.sprintf "invalid type: %s" typ
      in
      Printf.printf "'const' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "add" ->
      let dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      let instr = Arithmetic (Add (arg1, arg2)) in
      Printf.printf "'add' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "mul" ->
      let dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      let instr = Arithmetic (Mul (arg1, arg2)) in
      Printf.printf "'add' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "sub" ->
      let dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      let instr = Arithmetic (Sub (arg1, arg2)) in
      Printf.printf "'add' instruction, dest: %s, instr: %s" dest @@ show instr;
      instr
    | "div"
    | "eq"
    | "lt"
    | "Gt"
    | "Le"
    | "Ge"
    | "not"
    | "and"
    | "or"
    | "jmp"
    | "Br"
    | "call"
    | "ret"
    | "id"
    | "print"
    | "nop"
    | _ -> failwith @@ Printf.sprintf "of not implemented: %s" op)
;;
