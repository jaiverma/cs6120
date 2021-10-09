module J = Yojson.Safe

type t = Func of Func.t list

let of_file filename =
  let json = J.from_file filename in
  let functions = J.Util.member "functions" json |> J.Util.to_list in
  List.map
    (fun f ->
      let instrs = J.Util.member "instrs" f |> J.Util.to_list in
      List.map Instr.of_yojson instrs)
    functions
;;
