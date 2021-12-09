type direction = Forward | Up | Down
type step = direction * int

let direction_of_string = function
    | "forward" -> Forward
    | "up" -> Up
    | "down" -> Down
    | x -> failwith ("Unknown direction " ^ x)

let parse_line l =
    let dir::dist::_ = Utils.split_by_string " " l in
    (direction_of_string dir, int_of_string dist)

let rec travel horizontal depth = function
    | [] -> (horizontal, depth)
    | (direction, dist)::xs ->
        match direction with
            | Forward -> travel (horizontal + dist) depth xs
            | Up -> travel horizontal (depth - dist) xs
            | Down -> travel horizontal (depth + dist) xs

let rec travel2 horizontal depth aim = function
    | [] -> (horizontal, depth)
    | (direction, dist)::xs ->
        match direction with
            | Forward -> travel2 (horizontal + dist) (depth + aim*dist) aim xs
            | Up -> travel2 horizontal depth (aim - dist) xs
            | Down -> travel2 horizontal depth (aim + dist) xs

let result1 = "inputs/day2.dat"
    |> Utils.read_lines
    |> List.map parse_line
    |> travel 0 0

let () = print_endline ("Result 1 " ^ (string_of_int ((fst result1) * (snd result1))))

let result2 = "inputs/day2.dat"
    |> Utils.read_lines
    |> List.map parse_line
    |> travel2 0 0 0

let () = print_endline ("Result 2 " ^ (string_of_int ((fst result2) * (snd result2))))