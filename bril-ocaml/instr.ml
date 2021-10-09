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
      Printf.printf "'mul' instruction, dest: %s, instr: %s\n" dest @@ show instr;
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
      Printf.printf "'sub' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "div" ->
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
      let instr = Arithmetic (Div (arg1, arg2)) in
      Printf.printf "'div' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "eq" ->
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
      let instr = Comparison (Eq (arg1, arg2)) in
      Printf.printf "'eq' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "lt" ->
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
      let instr = Comparison (Lt (arg1, arg2)) in
      Printf.printf "'lt' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "gt" ->
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
      let instr = Comparison (Gt (arg1, arg2)) in
      Printf.printf "'gt' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "le" ->
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
      let instr = Comparison (Le (arg1, arg2)) in
      Printf.printf "'le' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "ge" ->
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
      let instr = Comparison (Ge (arg1, arg2)) in
      Printf.printf "'ge' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "not" ->
      let dest = J.Util.member "dest" json |> J.Util.to_string in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let arg =
        match args with
        | [ a ] -> a
        | _ -> failwith "expecting one argument"
      in
      let instr = Logic (Not arg) in
      Printf.printf "'not' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "and" ->
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
      let instr = Logic (And (arg1, arg2)) in
      Printf.printf "'and' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "or" ->
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
      let instr = Logic (Or (arg1, arg2)) in
      Printf.printf "'or' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
    | "jmp" ->
      let label = J.Util.member "label" json |> J.Util.to_string in
      let instr = Control (Jmp (Label label)) in
      Printf.printf "'or' instruction, instr: %s\n" @@ show instr;
      instr
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
      let instr =
        match labels with
        | [ a; b ] -> Control (Br (arg, Label a, Label b))
        | _ -> failwith "expecting two labels"
      in
      Printf.printf "'br' instruction, instr: %s\n" @@ show instr;
      instr
    | "call" ->
      let funcs =
        J.Util.member "funcs" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let func =
        match funcs with
        | [ a ] -> a
        | _ -> failwith "expecting one argument"
      in
      let dest = J.Util.member "dest" json |> J.Util.to_string in
      let args =
        J.Util.member "args" json |> J.Util.to_list |> List.map J.Util.to_string
      in
      let _typ = J.Util.member "type" json |> J.Util.to_string in
      let instr = Control (Call (func, args)) in
      Printf.printf "'call' instruction, dest: %s, instr: %s\n" dest @@ show instr;
      instr
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
      let instr = Control (Ret arg) in
      Printf.printf "'ret' instruction, instr: %s\n" @@ show instr;
      instr
    | "id" | "print" | "nop" | _ -> failwith @@ Printf.sprintf "op not implemented: %s" op)
;;
