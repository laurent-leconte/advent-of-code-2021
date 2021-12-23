type cuboid = | Cube of int * int * int * int * int * int | None

module Points = Set.Make(struct
                            type t = int*int*int
                            let compare = compare
                        end)

let inter_cube c = function
    | None -> None
    | Cube(x1, x2, y1, y2, z1, z2) ->
        match c with
            | None -> None
            | Cube(a1, a2, b1, b2, c1, c2) ->
                let t1 = max x1 a1 in
                let t2 = min x2 a2 in
                let u1 = max y1 b1 in
                let u2 = min y2 b2 in
                let v1 = max z1 c1 in
                let v2 = min z2 c2 in
                if t1 > t2 || u1 > u2 || v1 > v2 
                then None
                else Cube(t1, t2, u1, u2, v1, v2) 

let volume = function
    | None -> 0
    | Cube(x1, x2, y1, y2, z1, z2) -> (x2 - x1 + 1)*(y2 - y1 + 1)*(z2 - z1 + 1)

let set_of_cuboid = function
    | None -> Points.empty
    | Cube(x1, x2, y1, y2, z1, z2) ->
        let s = ref Points.empty in
        for i = x1 to x2 do
            for j = y1 to y2 do
                for k = z1 to z2 do
                    s := Points.add (i, j, k) !s
                done
            done
        done;
        !s

let int_of_inst = function
    | "on" -> 1
    | "off" -> -1
    | _ -> failwith "unknown instruction"

let add_or_remove all_points = function
    | 1, c -> Points.union all_points (set_of_cuboid c)
    | -1, c -> Points.diff all_points (set_of_cuboid c)
    | _, _ -> failwith "wrong instruction"

let parse_line s =
    let parse_coord s =
        let _::range::_ = Utils.split_by_string "=" s in
        let first::last::_ = Utils.split_by_string  "\.\." range in
        (int_of_string first, int_of_string last) in
    let instruction::coords::_ = Utils.split_by_string " " s in
    let x::y::z::_ = Utils.split_by_string "," coords in
    let x1, x2 = parse_coord x in
    let y1, y2 = parse_coord y in
    let z1, z2 = parse_coord z in
    (int_of_inst instruction, Cube(x1, x2, y1, y2, z1, z2))

let part1 size input = 
    let core = Cube(-size, size, -size, size, -size, size) in
    let crop_to_core (inst, c) = (inst, inter_cube core c) in
    input
        |> Utils.read_lines
        |> List.map parse_line
        |> List.map crop_to_core
        |> List.fold_left add_or_remove Points.empty 
        |> Points.cardinal

let remove_nones = List.filter (fun (i, c) -> c <> None)
let inter (_, c1) (s, c2) = (-s, inter_cube c1 c2) 

let rec build_intersections previous intersections = function
    | [] -> intersections
    | a::tl ->
        let new_inters = remove_nones @@ List.map (inter a) previous@intersections in
        build_intersections (a::previous) (intersections@new_inters) tl

let count_all instructions =
    let intersections = build_intersections [] [] instructions in 
    let add_volumes acc (s, c) = acc + s*volume c in
    let is_positive (s, _) = s > 0 in
    List.fold_left add_volumes 0 ((List.filter is_positive instructions)@intersections)

let () = Printf.printf "Part1 : %d\n" (part1 50 "inputs/day22.dat")