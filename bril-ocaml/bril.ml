let () =
  let filename = "/tmp/cs6120/add.json" in
  let functions = Program.of_file filename in
  List.iter (fun fn -> Printf.printf "function: %s\n" @@ Func.show fn) functions
;;
