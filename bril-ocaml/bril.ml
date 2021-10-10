let () =
  let filename = "/tmp/cs6120/add.json" in
  let functions = Program.of_file filename in
  List.iter (fun fn -> Printf.printf "function: %s\n" @@ Func.show fn) functions;
  (* print basic blocks for each function *)
  List.iter
    (fun (fn : Func.t) ->
      Printf.printf "function: %s\n" fn.name;
      let bbs : Instr.t list list = Program.get_bbs fn in
      List.iter
        (fun bb ->
          Printf.printf "block:\n";
          List.iter (fun instr -> Printf.printf "\t%s\n" @@ Instr.show instr) bb)
        bbs)
    functions
;;
