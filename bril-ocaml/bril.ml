module F = Func

let () =
  let filename = "/tmp/cs6120/add.json" in
  let instrs = Program.of_file filename in
  List.iter (fun instr -> Printf.printf "%s\n" @@ Instr.show instr) @@ List.flatten instrs
;;
