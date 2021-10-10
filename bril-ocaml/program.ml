module J = Yojson.Safe

type t = Func of Func.t list

let of_file filename =
  let json = J.from_file filename in
  let functions = J.Util.member "functions" json |> J.Util.to_list in
  List.map
    (fun f ->
      let instrs =
        J.Util.member "instrs" f |> J.Util.to_list |> List.map Instr.of_yojson
      in
      let name = J.Util.member "name" f |> J.Util.to_string in
      let args =
        J.Util.member "args" f
        |> J.Util.to_list
        |> List.map (fun arg ->
               let arg_name = J.Util.member "name" arg |> J.Util.to_string in
               let arg_type = J.Util.member "type" arg |> J.Util.to_string in
               arg_name, arg_type)
      in
      let typ =
        match List.find_opt (fun key -> key = "type") @@ J.Util.keys f with
        | Some _ -> Some (J.Util.member "type" f |> J.Util.to_string)
        | None -> None
      in
      { Func.name; args; instrs; typ })
    functions
;;
