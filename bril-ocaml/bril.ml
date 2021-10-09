module F = Func

let () =
  let filename = "/tmp/cs6120/add.json" in
  let _instrs = Program.of_file filename in
  print_endline "hello world"
;;
