type node = int * int
(* an edge is a weight and a destination node *)
type edge = int * node

module Nodes = Set.Make(struct
                            type t = int*int
                            let compare = compare
                        end)

let graph_of_matrix (mat, m, n) =
    let within_bounds (i,j) = (i >= 0) && (i < m) && (j >= 0) && (j < n) in
    let neighbors (i,j) = [(i-1,j); (i,j-1); (i,j+1); (i+1,j)] |> List.filter within_bounds in
    let add_node i j g mij =
        let neighbors = neighbors (i,j) in
        let edges = List.map (fun (x,y) -> (mat.(x).(y), (x,y))) neighbors in
        Hashtbl.add g (i,j) edges; g in
    let graph = Hashtbl.create (m*n) in 
    Utils.foldij add_node graph mat

let tile x (mat, m, n) =
    let res = Array.make_matrix (x*m) (x*n) 0 in
    for i = 0 to x*m - 1 do
        for j = 0 to x*n - 1 do begin
            let ii = i / m in
            let jj = j / n in
            let remi = i mod m in
            let remj = j mod n in
            res.(i).(j) <- (mat.(remi).(remj) + ii + jj -1) mod 9 + 1
            end
        done
    done;
    (res, x*m, x*n)

let dijkstra start graph = 
    let compare_second (n1, d1) (n2, d2) = compare d1 d2 in
    let update t (n, d) = Hashtbl.add t n d; t in
    (* takes a Hashtbl of distances so far, a set of visited nodes, and a list of (node, distance) points to visit *)
    let rec loop distances visited = function
        | [] -> distances
        | (a,d)::tl ->
            (* if current node was already visited, skip it *)
            if Nodes.mem a visited then loop distances visited tl else
            (* mark current node as visited *)
            let new_visited = Nodes.add a visited in
            let not_visited (w, n) = Bool.not @@ Nodes.mem n visited in
            (* get all non-visited neighbors *)
            let eligible_edges = Hashtbl.find graph a |> List.filter not_visited in 
            (* for each neighbor, the new distance is the min between the previous distance and the new path *)
            let new_distance (w,n) =
                let tentative_dist = match Hashtbl.find_opt distances n with
                    | None -> d + w
                    | Some(previous) -> min previous (d + w) in
                (n, tentative_dist) in
            (* build a list of all enighbors with updated distances *)
            let nodes_and_distances = List.map new_distance eligible_edges in
            (* update the Hashtbl of distances with the new values *)
            let new_distances = List.fold_left update distances nodes_and_distances in
            (* add the neighbors to the list of nodes to visit and sort by distance *)
            let to_visit = List.sort compare_second (nodes_and_distances@tl) in
            loop new_distances new_visited to_visit in
    let initial_distances = Hashtbl.create 1 in
    let empty = Nodes.empty in
    loop initial_distances empty [(start,0)]

(* part 1 *)
let distances = "inputs/day15.dat" |> Utils.read_matrix |> graph_of_matrix |> dijkstra (0,0)
let () = print_endline @@ string_of_int @@ Hashtbl.find distances (99,99)

(* part 2 *)
let distances = "inputs/day15.dat" |> Utils.read_matrix |> tile 5 |> graph_of_matrix |> dijkstra (0,0)
let () = print_endline @@ string_of_int @@ Hashtbl.find distances (499,499)