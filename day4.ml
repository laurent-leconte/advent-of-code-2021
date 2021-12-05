type cell = {
    number: int;
    marked: bool;
}

type board = cell list list

let cell_value c = c.number
let is_marked cell = cell.marked
let is_unmarked cell = Bool.not (is_marked cell)

let print_line l = l |> List.map cell_value |> List.map string_of_int |> String.concat " " |> print_endline

let print_board = List.map print_line

let rec mark_line number = function
    | [] -> []
    | c::tl -> 
        if number == c.number then
            {number = number; marked = true}::tl
        else c::(mark_line number tl)

let rec mark_board number = function
    | [] -> []
    | hd::tl -> (mark_line number hd)::(mark_board number tl)

let is_winning_board board =
    let is_winning_line l =
        List.for_all is_marked l in
    let horizontal = List.exists is_winning_line board in
    let vertical = List.exists is_winning_line (Utils.transpose board) in
    horizontal || vertical

let score_board b = 
    let filter_line = List.filter is_unmarked in
    let score_line l = l |> filter_line |> List.map cell_value |> List.fold_left (+) 0 in
    b |> List.map score_line |> List.fold_left (+) 0

let parse_input file =
    let raw_lines = Utils.read_lines file in
    let rec split_on_empty_lines acc = function
        | [] -> [List.rev acc]
        | l::tl ->
            if String.length l == 0 then
                (List.rev acc)::(split_on_empty_lines [] tl)
            else split_on_empty_lines (l::acc) tl in
    let [first_line_list]::boards_lists = split_on_empty_lines [] raw_lines in
    let numbers = first_line_list |> Utils.split_by_string "," |> List.map int_of_string in
    let cell_of_int i = {number = i; marked = false} in
    let make_line s = s 
        |> String.trim  (* remove leading spaces in case of single-digit first number *)
        |> Utils.split_by_string " +" (* split on whitespace *)
        |> List.map int_of_string 
        |> List.map cell_of_int in
    let make_board = List.map make_line in
    let boards = List.map make_board boards_lists in
    (numbers, boards)

let fork predicate l =
    let rec loop acc_true acc_false = function
        | [] -> (List.rev acc_true, List.rev acc_false)
        | hd::tl -> 
            if predicate hd then
                loop (hd::acc_true) acc_false tl
            else loop acc_true (hd::acc_false) tl in
    loop [] [] l

let winning_boards (numbers, boards) =
    let rec play boards = function
        | [] -> []
        | n::tl -> 
            let updated_boards = List.map (mark_board n) boards in
            let (winners, losers) = fork is_winning_board updated_boards in
            if List.length winners > 0 then
                (n, List.hd winners)::(play losers tl)
            else play updated_boards tl in
    play boards numbers

let day4 input =
    let won = input |> parse_input |> winning_boards in
    let first_won = List.hd won in
    let last_won = List.hd @@ List.rev won in
    let score (number, board) = number * score_board board in
    print_endline @@ "First game won: " ^ (string_of_int @@ score first_won);
    print_endline @@ "Last game won: " ^ (string_of_int @@ score last_won)


let () = day4 "day4.dat"