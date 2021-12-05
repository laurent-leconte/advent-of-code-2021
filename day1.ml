let example = [199;200;208;210;200;207;240;269;260;263]

let count_increasing =
    let rec loop acc = function
        | [] -> acc
        | [x] -> acc
        | x :: (y :: xs as tl) ->
            if x < y then loop (acc + 1) tl else loop acc tl in
    loop 0

let sum_three soundings = 
    let rec loop res = function
        | [] | [_] | _ :: _ :: [] -> res
        | x :: (y :: z :: xs as tl) ->
            loop ((x + y + z)::res) tl in
    List.rev (loop [] soundings)

let result1 = "day1.dat"
    |> Utils.read_lines
    |> List.map int_of_string
    |> count_increasing
    |> string_of_int

let result2 = "day1.dat"
    |> Utils.read_lines
    |> List.map int_of_string
    |> sum_three
    |> count_increasing
    |> string_of_int

let () = print_endline ("Result 1 : " ^ result1)
let () = print_endline ("Result 2 : " ^ result2)