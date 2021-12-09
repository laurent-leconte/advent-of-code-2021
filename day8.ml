module StringSet = Set.Make(String)

let parse_line l =
    let first::second::_ = Utils.split_by_string " | "l in
    let split_digits = Utils.split_by_string " " in
    (split_digits first, split_digits second)

let set_of_string s = StringSet.of_list (Utils.explode s)

let count_easy_digits (unique, output) =
    let is_easy d = 
        let n = String.length d in
        (n == 2) || (n == 3) || (n == 4) || (n == 7) in
    let add_if_easy acc d = if is_easy d then acc + 1 else acc in
    List.fold_left add_if_easy 0 output

let string_of_set s = StringSet.fold (fun s acc -> s ^ acc) s ""

let print_map h =
    let f k v acc = acc ^ k ^ ": " ^ (string_of_int v) ^ "\n" in
    print_endline @@ Hashtbl.fold f h ""

let map_digits l =
    let set_to_int = Hashtbl.create 10 in
    let add s i = Hashtbl.add set_to_int (string_of_set s) i in
    let of_size n s = (StringSet.cardinal s == n) in
    let one = List.find (of_size 2) l in
    let seven = List.find (of_size 3) l in
    let four = List.find (of_size 4) l in
    let eight = List.find (of_size 7) l in
    add one 1;
    add eight 8;
    add four 4;
    add seven 7;
    let contains s1 s2 = StringSet.diff s1 s2 == StringSet.empty in
    let is_nine s = (of_size 6 s) && (contains four s) in
    let nine = List.find is_nine l in
    add nine 9;
    let is_three s = (of_size 5 s) && (contains one s) in
    let three = List.find is_three l in
    add three 3;
    let is_zero s = (of_size 6 s) && (contains seven s) && not (contains four s) in
    let zero = List.find is_zero l in
    add zero 0;
    let is_six s = (of_size 6 s) && not (contains seven s) && not (contains four s) in
    let six = List.find is_six l in
    add six 6;
    let is_five s = (of_size 5 s) && (contains s six) in
    add (List.find is_five l) 5;
    let is_two s = (of_size 5 s) && not (StringSet.equal three s) && not (contains s six) in
    add (List.find is_two l) 2;
    set_to_int

let list_to_int = List.fold_left (fun acc x -> acc*10 + x) 0

let () = "inputs/day8.dat"
    |> Utils.read_lines
    |> List.map parse_line
    |> List.map count_easy_digits
    |> List.fold_left (+) 0
    |> string_of_int
    |> print_endline

let day8_2 (unique, output) =
    let set_to_int = map_digits (List.map set_of_string unique) in
    let result = output 
        |> List.map set_of_string  
        |> List.map string_of_set
        |> List.map (Hashtbl.find set_to_int)
        |> list_to_int in
    result

let () = "inputs/day8.dat"
    |> Utils.read_lines
    |> List.map parse_line
    |> List.map day8_2
    |> List.fold_left (+) 0
    |> string_of_int
    |> print_endline