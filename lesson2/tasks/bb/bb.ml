open Core

(* Function to parse instructions and generate basic-blocks*)
let parse_instrs instrs =
    let open Yojson.Basic.Util in
    let rec parse_instrs_impl blocks block = function
        | [] -> List.filter ~f:(fun l -> List.length l > 0) block :: blocks
                |> List.rev
        | x::xs ->
            (* Each  instruction will have an "op" key *)
            let op = match (member "op" x) with
                | `Null -> "label"
                | `String x -> x
                | _ -> failwith "op should always be of type `String"
            in
            match op with
            (* Check for a label. Label instructions don't have the "op" key *)
            | "label" ->
                printf "found a label\n";
                parse_instrs_impl (block :: blocks) [] xs

            (* check for control-flow instructions which would mark the end of
               a basic block *)
            | "jmp" | "br" | "ret" ->
                parse_instrs_impl ((to_assoc x :: block) :: blocks) [] xs

            (* All other instructions *)
            | _ ->
                parse_instrs_impl blocks (to_assoc x :: block) xs
    in

    parse_instrs_impl [] [] instrs

let rec print_bb = function
    | [] -> ()
    | x::xs ->
        `Assoc x |> Yojson.Basic.to_string |> printf "\t%s\n";
        print_bb xs

let () =
    let open Yojson.Basic.Util in
    Yojson.Basic.from_file "add.json"
    |> member "functions"
    |> to_list
    |> List.map ~f:(fun fn ->
        let fn_name = fn |> member "name" |> to_string in
        let bbs = fn |> member "instrs" |> to_list |> parse_instrs
        in (fn_name, bbs)
    )
    |> List.iter ~f:(fun (fn_name, fn_bbs) ->
        printf "Processing function : %s\n" fn_name;
        List.iter ~f:(fun bb ->
            if List.length bb > 0 then
            begin
                printf "new bb:\n";
                print_bb bb
            end
            else
                ()) fn_bbs
    )
