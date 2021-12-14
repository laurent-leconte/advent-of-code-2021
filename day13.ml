type axis = X | Y
type instruction = axis * int

module Points = Set.Make(struct
                            type t = int*int
                            let compare = compare
                        end)

let axis_of_string = function
    | "x" -> X
    | "y" -> Y
    | s -> failwith ("Unknown direction " ^ s)

let print_points points =
    let list_of_points = Points.elements points in
    let max_coord (xmax, ymax) (x,y) = ((max xmax x), (max ymax y)) in
    let (xmax, ymax) = List.fold_left max_coord (List.hd list_of_points) list_of_points in
    let mat = Array.make_matrix (ymax + 1) (xmax + 1) "." in
    let add_point (x,y) = mat.(y).(x) <- "#" in
    let () = List.iter add_point list_of_points in
    Utils.print_matrix Fun.id mat

let fold (axis,l) =
    let fold_one (x,y) =
    match axis with
        | X -> if x > l then (2*l - x, y) else (x,y)
        | Y -> if y > l then (x, 2*l - y) else (x,y) in
    Points.map fold_one

(* split the input file between points and directions *)
let rec split_lines points = function
    | [] -> (points, [])
    | a::tl -> if a = "" then (points, tl) else split_lines (a::points) tl

let parse_directions s =
    let t = List.nth (Utils.split_by_string " " s) 2 in
    let axis::line::_ = Utils.split_by_string "=" t in
    (axis_of_string axis, int_of_string line)

let parse_input file =
    let (raw_points, raw_directions) = file |> Utils.read_lines |> split_lines [] in
    let point_of_string s = 
        let x::y::_ = Utils.split_by_string "," s in
        (int_of_string x, int_of_string y) in
    let add points (x,y) = Points.add (x,y) points in
    let points = raw_points |> List.map point_of_string |> List.fold_left add Points.empty in
    let directions = raw_directions |> List.map parse_directions in
    (points, directions)

let part1 file =
    let (points, directions) = parse_input file in
    Printf.printf "%n\n" (Points.cardinal @@ fold (List.hd directions) points)

let part2 file =
    let (points, directions) = parse_input file in
    let rec loop points = function
        | [] -> points
        | a::tl -> 
            let folded = fold a points in
            loop folded tl in
    let folded = loop points directions in
    print_points folded

let () = part1 "inputs/day13.dat"
let () = part2 "inputs/day13.dat"