type point = int * int
type line = point * point

let parse_line s =
    let parse_coord c = 
        let x::y::_ = Utils.split_by_string "," c in
        (int_of_string x, int_of_string y) in
    let c1::c2::_ = Utils.split_by_string " -> " s in
    (parse_coord c1, parse_coord c2)

let make_range a b = 
    let start = min a b in
    let stop = max a b in
    List.init (stop - start + 1) (fun x -> start + x)

let diagonal (c1, c2) = 
    let x1, y1 = c1 in
    let x2, y2 = c2 in
    if x1 + y2 == x2 + y1 then
        List.map (fun i -> (x1 + i, y1 + i)) (make_range 0 (x2 - x1))
    else if x1 + y1 == x2 + y2 then
        List.map (fun i -> (x1 + i, y1 - i)) (make_range 0 (x2 - x1))
    else []

let points_of_line include_diagonal (c1, c2) =
    let x1, y1 = c1 in
    let x2, y2 = c2 in
    if x1 == x2 then
        List.map (fun y -> (x1, y))  (make_range y1 y2)
    else if y1 == y2 then
        List.map (fun x -> (x, y1)) (make_range x1 x2)
    else if include_diagonal then
        diagonal (c1, c2)
    else []

let trace_lines diagonal lines =
    (* find grid dimensions *)
    let max_of_3 a b c = max (max a b) c in
    let rec find_max h v = function
        | [] -> (h, v)
        | (c1, c2)::tl ->
            let h1, v1 = c1 in
            let h2, v2 = c2 in
            find_max (max_of_3 h h1 h2) (max_of_3 v v1 v2) tl in
    let hmax, vmax = find_max 0 0 lines in
    (* buid empty ground matrix *)
    let ground = Array.make_matrix (vmax + 1) (hmax + 1) 0 in
    (* take a list of points and mark them on the ground *)
    let rec trace_one_line mat = function
        | [] -> mat
        | (x,y)::tl ->
            mat.(y).(x) <- mat.(y).(x) + 1;
            trace_one_line mat tl in
    List.fold_left trace_one_line ground (List.map (points_of_line diagonal) lines)

let count_overlaps ground =
    (* Utils.print_int_array ground; *)
    let count acc cell = if cell > 1 then acc + 1 else acc in
    let count_line = Array.fold_left count 0 in
    Array.fold_left (+) 0 (Array.map count_line ground)

let day5 diagonal input = 
    input
        |> Utils.read_lines
        |> List.map parse_line
        |> trace_lines diagonal
        |> count_overlaps
        |> string_of_int
        |> print_endline

let () = day5 false "inputs/day5.dat"
let () = day5 true "inputs/day5.dat"