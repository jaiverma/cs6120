open Core

module BasicBlock = struct
    let _label = ref "0"

    (* label (string), basic-block (list of instructions) *)
    type t = {
        label: string;
        instrs: (string * Yojson.Basic.t) list list
    }

    let make_bb ?label:(label=(!_label)) instrs =
        if String.equal label !_label then
            _label := string_of_int (int_of_string (!_label) + 1)
        else
            ();
        { label=sprintf "L%s" label; instrs=instrs }

    let add_instr block instr =
        { label=block.label; instrs=block.instrs @ [instr] }

    let print_bb block =
        let instrs = block.instrs in
        let rec print_instr = function
            | [] -> ()
            | x::xs ->
                `Assoc x |> Yojson.Basic.to_string |> printf "\t%s\n";
                print_instr xs
        in
        print_instr instrs

    let is_empty block = List.length block.instrs = 0

    let size block = List.length block.instrs
end

(* Function to parse instructions and generate basic-blocks*)
let parse_instrs instrs =
    let open Yojson.Basic.Util in
    let rec parse_instrs_impl ~blocks ~block = function
        | [] -> List.filter ~f:(fun block -> not (BasicBlock.is_empty block))
            (block :: blocks) |> List.rev
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
                parse_instrs_impl ~blocks:(block :: blocks)
                    ~block:(BasicBlock.make_bb
                        ~label:(member "label" x |> to_string) []) xs

            (* check for control-flow instructions which would mark the end of
               a basic block *)
            | "jmp" | "br" | "ret" ->
                parse_instrs_impl
                    ~blocks:(
                        (BasicBlock.add_instr block (to_assoc x)) :: blocks)
                    ~block:(BasicBlock.make_bb []) xs

            (* All other instructions *)
            | _ ->
                parse_instrs_impl ~blocks:blocks
                    ~block:(BasicBlock.add_instr block (to_assoc x)) xs
    in

    parse_instrs_impl ~blocks:[] ~block:(BasicBlock.make_bb []) instrs

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
        in (fn_name, bbs))
    |> List.iter ~f:(fun (fn_name, fn_bbs) ->
        printf "Processing function : %s\n" fn_name;
        List.iter ~f:(fun bb -> begin
                printf "new bb:\n";
                printf "bb size: %d\n" (BasicBlock.size bb);
                BasicBlock.print_bb bb
            end
        ) fn_bbs)
