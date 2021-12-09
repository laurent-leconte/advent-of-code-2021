type criterion = Oxygen | CO2

let example = ["00100";"11110";"10110";"10111";"10101";"01111";"00111";"11100";"10000";"11001";"00010";"01010"]

let print_int_list l = 
    let inner = String.concat ";" (List.map string_of_int l) in
    print_endline ("[" ^ inner ^ "]")

let most_common_bit l =
    let n = List.length l in
    let sum = List.fold_left (+) 0 l in
    let res = (if sum*2 >= n then 1 else 0) in
    (* print_int_list l;
    print_endline ("most common bit: " ^ string_of_int res); *)
    res

let int_of_binary_list l =
    let rec loop acc pow = function
        | [] -> acc
        | x::xs -> loop (acc + x*pow) (pow*2) xs in
    loop 0 1 (List.rev l)

let gamma_epsilon input =
    let gamma_list = input 
        |> List.map Utils.explode
        |> List.map (List.map int_of_string)
        |> Utils.transpose
        |> List.map most_common_bit in
    let rec flip_bits = function
        | [] -> []
        | x::xs -> (if x == 0 then 1 else 0) :: flip_bits xs in
    let epsilon_list = flip_bits gamma_list in 
    (int_of_binary_list gamma_list, int_of_binary_list epsilon_list)

let test_bit pos bit l = (List.nth l pos) == bit

let rec print_list_of_lists = function
    | [] -> ()
    | hd::tl -> 
        print_endline @@ String.concat "" (List.map string_of_int hd);
        print_list_of_lists tl

let life_support_rating crit numbers =
    let test pos most_common =
        match crit with
        | Oxygen -> test_bit pos most_common
        | CO2 -> test_bit pos (1 - most_common) in
    let rec loop pos = function
        | [] -> failwith "life_support_rating: no more numbers"
        | [a] -> int_of_binary_list a
        | l -> 
            let most_common = most_common_bit (List.nth (Utils.transpose l) pos) in
            let filtered = List.filter (test pos most_common) l in
            loop (pos + 1) filtered in
    loop 0 numbers


let day3_1 input =
    let (gamma, epsilon) = input |> Utils.read_lines |> gamma_epsilon in
    gamma*epsilon

let day3_2 input =
    let numbers = input 
        |> Utils.read_lines
        |> List.map Utils.explode
        |> List.map (List.map int_of_string) in
    let ox = life_support_rating Oxygen numbers in
    let co2 = life_support_rating CO2 numbers in
    ox*co2

let () = print_endline @@ string_of_int @@ day3_1 "inputs/day3.dat"

let () = print_endline @@ string_of_int @@ day3_2 "inputs/day3.dat"
