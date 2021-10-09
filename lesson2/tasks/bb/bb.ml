open Core

module BasicBlock = struct
  let _label = ref "0"

  (* label (string), basic-block (list of instructions) *)
  type t =
    { label : string
    ; instrs : (string * Yojson.Basic.t) list list
    }

  let make_bb ?(label = sprintf "L%s" !_label) instrs =
    if String.equal label !_label
    then _label := string_of_int (int_of_string !_label + 1)
    else ();
    { label = sprintf "%s" label; instrs }
  ;;

  let add_instr block instr = { label = block.label; instrs = block.instrs @ [ instr ] }

  let print_bb block =
    let instrs = block.instrs in
    let rec print_instr = function
      | [] -> ()
      | x :: xs ->
        `Assoc x |> Yojson.Basic.to_string |> printf "\t%s\n";
        print_instr xs
    in
    print_instr instrs
  ;;

  let is_empty block = List.length block.instrs = 0
  let size block = List.length block.instrs
end

(* Function to convert list of basic-blocks into an association list of the format:
[(label, bb), ...] *)
let rec to_assoc_list (bb : BasicBlock.t list) =
  match bb with
  | [] -> []
  | x :: xs -> (x.label, x) :: to_assoc_list xs
;;

module CFG = struct
  type t =
    { fn : string
    ; bbs : (string * BasicBlock.t) list
    ; edges : (string * string) list
    }

  let make_cfg ~fn ~blocks =
    let open Yojson.Basic.Util in
    let edges = ref [] in
    let rec build_cfg (bbs' : BasicBlock.t list) =
      match bbs' with
      | [] -> ()
      | bb :: bbs ->
        (* Get last instruction of block *)
        let last = `Assoc (List.last_exn bb.instrs) in
        let _ =
          match last |> member "op" |> to_string with
          | "jmp" ->
            (* jmp is guaranteed to have only one target label *)
            let target =
              match last |> member "labels" |> to_list with
              | [ x ] -> x |> to_string
              | _ -> failwith "jmp can't have more than one target..."
            in
            edges := (bb.label, target) :: !edges
          | "br" ->
            (* br will always have 2 possible target labels *)
            let target_a, target_b =
              match last |> member "labels" |> to_list with
              | [ a; b ] -> a |> to_string, b |> to_string
              | _ -> failwith "br can only have two targets..."
            in
            edges := (bb.label, target_a) :: (bb.label, target_b) :: !edges
          | "ret" -> () (* ret will not have any successors *)
          | _ ->
            (* This could be a fallthrough edge, if a block remains in
                    the list then add an edge to that *)
            (match bbs with
            | [] -> ()
            | x :: _ ->
              let target = x.label in
              edges := (bb.label, target) :: !edges)
        in
        build_cfg bbs
    in
    build_cfg blocks;
    { fn; bbs = to_assoc_list blocks; edges = !edges }
  ;;

  let print_nodes cfg =
    let nodes = List.map ~f:(fun (label, _) -> label) cfg.bbs in
    let rec print_nodes_impl = function
      | [] -> ()
      | x :: xs ->
        printf "\t%s\n" x;
        print_nodes_impl xs
    in
    print_nodes_impl nodes
  ;;

  let print_edges cfg =
    let edges = cfg.edges in
    let rec print_edges_impl = function
      | [] -> ()
      | (a, b) :: xs ->
        printf "\t%s -> %s\n" a b;
        print_edges_impl xs
    in
    print_edges_impl edges
  ;;

  let dot cfg =
    let rec get_edges = function
      | [] -> []
      | (a, b) :: xs -> sprintf "\t%s -> %s;\n" a b :: get_edges xs
    in
    let edges = get_edges cfg.edges in
    let nodes = List.map ~f:(fun (label, _) -> sprintf "\t%s;\n" label) cfg.bbs in
    let filename = sprintf "/tmp/cs6120/%s.dot" cfg.fn in
    let oc = Out_channel.create filename in
    Out_channel.output_string oc "digraph G {\n";
    (* add nodes *)
    List.iter ~f:(fun node -> Out_channel.output_string oc node) nodes;
    (* add edges *)
    List.iter ~f:(fun edge -> Out_channel.output_string oc edge) edges;
    Out_channel.output_string oc "}\n";
    Out_channel.close oc;
    filename
  ;;
end

(* Function to parse instructions and generate basic-blocks*)
let parse_instrs instrs =
  let open Yojson.Basic.Util in
  let rec parse_instrs_impl ~blocks ~block = function
    | [] ->
      List.filter ~f:(fun block -> not (BasicBlock.is_empty block)) (block :: blocks)
      |> List.rev
    | x :: xs ->
      (* Each  instruction will have an "op" key *)
      let op =
        match member "op" x with
        | `Null -> "label"
        | `String x -> x
        | _ -> failwith "op should always be of type `String"
      in
      (match op with
      (* Check for a label. Label instructions don't have the "op" key *)
      | "label" ->
        printf "found a label\n";
        parse_instrs_impl
          ~blocks:(block :: blocks)
          ~block:(BasicBlock.make_bb ~label:(member "label" x |> to_string) [])
          xs
      (* check for control-flow instructions which would mark the end of
               a basic block *)
      | "jmp" | "br" | "ret" ->
        parse_instrs_impl
          ~blocks:(BasicBlock.add_instr block (to_assoc x) :: blocks)
          ~block:(BasicBlock.make_bb [])
          xs
      (* All other instructions *)
      | _ -> parse_instrs_impl ~blocks ~block:(BasicBlock.add_instr block (to_assoc x)) xs)
  in
  parse_instrs_impl ~blocks:[] ~block:(BasicBlock.make_bb []) instrs
;;

let rec print_bb = function
  | [] -> ()
  | x :: xs ->
    `Assoc x |> Yojson.Basic.to_string |> printf "\t%s\n";
    print_bb xs
;;

let () =
  let open Yojson.Basic.Util in
  let fn_map =
    Yojson.Basic.from_file "add.json"
    |> member "functions"
    |> to_list
    |> List.map ~f:(fun fn ->
           let fn_name = fn |> member "name" |> to_string in
           let bbs = fn |> member "instrs" |> to_list |> parse_instrs in
           fn_name, bbs)
  in
  let cfg_map =
    List.map ~f:(fun (fn_name, bbs) -> CFG.make_cfg ~fn:fn_name ~blocks:bbs) fn_map
  in
  List.iter
    ~f:(fun cfg ->
      printf "function: %s\n" cfg.fn;
      printf "nodes:\n";
      CFG.print_nodes cfg;
      printf "edges:\n";
      CFG.print_edges cfg)
    cfg_map;
  List.iter
    ~f:(fun cfg ->
      printf "writing graph for function: %s\n" cfg.fn;
      let outfile = CFG.dot cfg in
      printf "\twritten to file: %s\n" outfile)
    cfg_map
;;
