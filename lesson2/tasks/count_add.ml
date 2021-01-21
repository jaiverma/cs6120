open Core

let read_bril ~filename =
    Yojson.Basic.from_file filename

(* Apply a function on each instruction *)
let rec process_instrs ~f = function
    | [] -> []
    | x::xs -> f x :: process_instrs ~f:f xs

let process_functions ~f bril =
    let open Yojson.Basic.Util in
    (* Get Functions from Program *)
    let functions = bril |> member "functions" |> to_list in

    (* For each function, count number of 'add' instructions *)
    List.iter ~f:(fun fn ->
        let name = member "name" fn |> to_string in
        let instrs = member "instrs" fn |> to_list in
        let ans = process_instrs ~f:f instrs in
        let add_count = List.fold_left ans ~f:(+) ~init:0 in
        printf "function [%s] :\n\tadd instr count : [%d]\n" name add_count
    ) functions

let () =
    let open Yojson.Basic.Util in

    (* Function which processes each instruction *)
    let count_add instr =
        let op = instr |> member "op" |> to_string in
        printf "[DEBUG] op : %s\n" op;
        match op with
        | "add" -> 1
        | _ -> 0
    in

    read_bril ~filename:"add.json"
    |> process_functions ~f:count_add
