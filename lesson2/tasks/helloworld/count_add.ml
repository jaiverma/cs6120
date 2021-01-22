open Core

let read_bril ~filename =
    Yojson.Basic.from_file filename

(* Apply a function on each instruction *)
let rec process_instrs ~f = function
    | [] -> []
    | x::xs -> f x @ process_instrs ~f:f xs

let process_functions ~f bril =
    let open Yojson.Basic.Util in
    (* Get Functions from Program *)
    let functions = bril |> member "functions" |> to_list in

    (* For each function, count number of 'add' instructions *)
    List.map ~f:(fun fn ->
        let name = member "name" fn |> to_string in
        let instrs = member "instrs" fn |> to_list in
        let ans = process_instrs ~f:f instrs in
        (name, ans)
    ) functions

let count_add () =
    let open Yojson.Basic.Util in

    (* Function which processes each instruction *)
    let count_add instr =
        let op = instr |> member "op" |> to_string in
        printf "[DEBUG] op : %s\n" op;
        match op with
        | "add" -> [1]
        | _ -> [0]
    in

    read_bril ~filename:"add.json"
    |> process_functions ~f:count_add
    |> List.iter ~f:(fun (fn_name, ans) ->
        let add_count = List.fold_left ans ~f:(+) ~init:0 in
        printf "function [%s] :\n\tadd instr count : [%d]\n" fn_name add_count)

let add_print () =
    let open Yojson.Basic.Util in

    (* Function to add a print instruction before each 'add' instruction *)
    let my_msg = `Assoc [
        ("op", `String "const");
        ("dest", `String "my_msg");
        ("type", `String "string");
        ("value", `String "Jai says hello!")
    ] in

    let print_instr = `Assoc [
        ("op", `String "print");
        ("args", `List [`String "my_msg"])
    ] in

    let add_print instr =
        let op = instr |> member "op" |> to_string in
        printf "[DEBUG] op : %s\n" op;
        match op with
        | "add" -> [print_instr; instr] (*((to_assoc print_instr) :: (to_assoc instr) :: [])*)
        | _ -> [instr]
    in

    let fns = read_bril ~filename:"add.json"
    |> process_functions ~f:add_print
    |> List.map ~f:(fun (fn_name, fn_body) ->
        `Assoc [
            ("name", `String fn_name);
            ("instrs", `List (my_msg :: fn_body))
        ])
    in

    let bril = `Assoc [
        ("functions", `List fns)
    ] in
    Yojson.Basic.to_file "new_add.json" bril

let () =
    count_add ();
    add_print ()
