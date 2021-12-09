let parse_input file =
    let data = List.hd @@ Utils.read_lines file in
    List.map int_of_string (Utils.split_by_string "," data)

let cost1 target positions =
    let unit_cost_function x = Int.abs (target - x) in
    List.fold_left (+) 0 (List.map unit_cost_function positions)

let cost2 target positions =
    let unit_cost_function x = 
        let dist = Int.abs (target - x) in
        dist * (dist + 1) / 2 in
    List.fold_left (+) 0 (List.map unit_cost_function positions)

let find_cheapest cost_function positions =
    let min, max = List.fold_left (fun (min, max) x ->
                                if x < min then (x, max) else
                                if x > max then (min, x) else
                                (min, max))
                            (List.hd positions, List.hd positions)
                            positions in
    let curried_cost_function x = cost_function x positions in
    let all_costs = List.map curried_cost_function (Utils.range min max) in
    List.fold_left (fun min x -> if x < min then x else min)
                  (List.hd all_costs)
                  all_costs


let day7 cost_function file =
    let positions = parse_input file in
    find_cheapest cost_function positions
 
let () = print_endline @@ string_of_int @@ day7 cost1 "inputs/day7.dat"
let () = print_endline @@ string_of_int @@ day7 cost2 "inputs/day7.dat"