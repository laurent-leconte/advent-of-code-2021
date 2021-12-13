type path = | Noloop of string list | Oneloop of string list

let peek = function
    | Noloop(l) | Oneloop(l) -> l

let map f = function
    | Noloop(l) -> Noloop(f l)
    | Oneloop(l) -> Oneloop(f l)

let make_graph edges =
    let graph = Hashtbl.create 10 in
    let split = Utils.split_by_string "-" in
    let add_edge u v =
        match Hashtbl.find_opt graph u with
            | None -> Hashtbl.add graph u [v]
            | Some l -> Hashtbl.replace graph u (v::l) in
    let do_one s =
        let u::v::_ = split s in
        add_edge u v;
        add_edge v u in
    List.iter do_one edges;
    graph

let is_lowercase s = String.lowercase_ascii s = s

(* a node can be added to a path if it's not a lower-case node or if it isn't already in it.
 "end" is handled as a special case and is excluded from normal eligible nodes *)
let is_eligible path node =
    if (node = "end" || node = "start") then
        false 
    else if (Bool.not @@ is_lowercase node) then
        true
    else
        match path with
            | Noloop(_) -> true
            | Oneloop(l) -> Bool.not @@ List.mem node l

(* for the given path, follow all vertices of the last node and:
    - optionally return a full path if one of the neighbors is "end" 
    - return a list of all new eligible paths, updating them from Noloop to Oneloop as necessary *)
let extend_path graph path =
    let neighbors = Hashtbl.find graph (List.hd @@ peek path) in
    let complete_path = 
        if List.mem "end" neighbors then 1 else 0 in
    let append_to_path x = function
        | Noloop(l) -> if (List.mem x l && is_lowercase x) then Oneloop(x::l) else Noloop(x::l)
        | Oneloop(l) -> Oneloop(x::l) in
    let new_paths = neighbors 
        |> List.filter (is_eligible path) 
        |> List.map (fun x -> (append_to_path x path)) in
    (complete_path, new_paths)

(* fond all paths till the "end" vertex by building out the list of partial paths passed as input *)
let rec all_paths graph found = function
    | [] -> found
    | l::tl -> 
        let (complete, new_paths) = extend_path graph l in
        all_paths graph (found + complete) (new_paths@tl)

let part1 input =
    let graph = input |> Utils.read_lines |> make_graph in
    all_paths graph 0 [Oneloop(["start"])]

let () = Printf.printf "Part 1: %n\n" (part1 "inputs/day12.dat")

let part2 input =
    let graph = input |> Utils.read_lines |> make_graph in
    all_paths graph 0 [Noloop(["start"])]

let () = Printf.printf "Part 2: %n\n" (part2 "inputs/day12.dat")
