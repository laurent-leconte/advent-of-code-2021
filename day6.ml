let example = [3;4;3;1;2]

let parse_input file =
    let line = List.hd @@ Utils.read_lines file in
    List.map int_of_string (Utils.split_by_string "," line)

let make_pop input =
    let pop = Array.make 9 0 in
    let update_one mat idx = 
        mat.(idx) <- mat.(idx) + 1; mat in
    List.fold_left update_one pop input

let simulate_one_day pop =
    let ready = pop.(0) in
    for i=1 to 8 do pop.(i-1) <- pop.(i); done;
    pop.(6) <- pop.(6) + ready;
    pop.(8) <- ready;
    pop

let rec simulate n pop =
    if n == 0 then pop else simulate (n-1) (simulate_one_day pop)

let day6 duration input =
    input |> make_pop |> simulate duration |> Array.fold_left (+) 0

let () = print_endline @@ string_of_int @@ day6 80 @@ parse_input "day6.dat"
let () = print_endline @@ string_of_int @@ day6 256 @@ parse_input "day6.dat"