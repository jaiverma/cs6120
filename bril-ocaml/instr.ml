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
  | Not of string
  | And of (string * string)
  | Or of (string * string)
[@@deriving show]

and control =
  | Jmp of t
  | Br of (string * t * t)
  | Call of (string * string list)
  | Ret of string option
[@@deriving show]

and misc =
  | Id of Typ.t
  | Print of string list
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
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let typ = J.Util.member "type" json |> J.Util.to_string in
      (match typ with
      | "int" ->
        let value = J.Util.member "value" json |> J.Util.to_int in
        Misc (Const (Primitive (Integer value)))
      | "bool" ->
        let value = J.Util.member "value" json |> J.Util.to_bool in
        Misc (Const (Primitive (Boolean value)))
      | _ -> failwith @@ Printf.sprintf "invalid type: %s" typ)
    | "add" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Arithmetic (Add (arg1, arg2))
    | "mul" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Arithmetic (Mul (arg1, arg2))
    | "sub" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Arithmetic (Sub (arg1, arg2))
    | "div" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Arithmetic (Div (arg1, arg2))
    | "eq" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Comparison (Eq (arg1, arg2))
    | "lt" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Comparison (Lt (arg1, arg2))
    | "gt" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Comparison (Gt (arg1, arg2))
    | "le" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Comparison (Le (arg1, arg2))
    | "ge" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Comparison (Ge (arg1, arg2))
    | "not" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg =
        match args with
        | [ a ] -> a
        | _ -> failwith "expecting one argument"
      in
      Logic (Not arg)
    | "and" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Logic (And (arg1, arg2))
    | "or" ->
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg1, arg2 =
        match args with
        | [ a; b ] -> a, b
        | _ -> failwith "expecting two arguments"
      in
      Logic (Or (arg1, arg2))
    | "jmp" ->
      let label = J.Util.member "label" json |> J.Util.to_string in
      Control (Jmp (Label label))
    | "br" ->
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg =
        match args with
        | [ a ] -> a
        | _ -> failwith "expecting one argument"
      in
      let labels =
        J.Util.member "labels" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      (match labels with
      | [ a; b ] -> Control (Br (arg, Label a, Label b))
      | _ -> failwith "expecting two labels")
    | "call" ->
      let funcs =
        J.Util.member "funcs" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let func =
        match funcs with
        | [ a ] -> a
        | _ -> failwith "expecting one argument"
      in
      let _dest = J.Util.member "dest" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      Control (Call (func, args))
    | "ret" ->
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg =
        match args with
        | [] -> None
        | [ a ] -> Some a
        | _ -> failwith "expecting at most one argument"
      in
      Control (Ret arg)
    | "id" -> failwith "id not implemented"
    | "print" ->
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      Misc (Print args)
    | "nop" -> Misc Nop
    | _ -> failwith @@ Printf.sprintf "op not implemented: %s" op)
;;
