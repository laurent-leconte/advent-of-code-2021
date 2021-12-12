type character = 
    | Closing of string
    | Opening of string

type parse_result = OK | Incomplete of character list | Corrupted of character

let string_of_character = function 
    | Opening(c) -> String.sub c 0 1
    | Closing(c) -> String.sub c 1 1

let string_of_result = function
    | OK -> "ok"
    | Incomplete(l) -> "incomplete " ^ (String.concat "" (List.map string_of_character l))
    | Corrupted(c) -> "illegal char " ^ (string_of_character c)

let character_of_string = function
    | "(" -> Opening("()")
    | "[" -> Opening("[]")
    | "{" -> Opening("{}")
    | "<" -> Opening("<>")
    | ")" -> Closing("()")
    | "]" -> Closing("[]")
    | "}" -> Closing("{}")
    | ">" -> Closing("<>")
    | x -> failwith @@ "unknown character " ^ x

let rec parse stack = function
    | [] -> if List.length stack == 0 then OK else Incomplete(stack)
    | a::tl -> 
        match a with
            | Opening(c) -> parse (a::stack) tl
            | Closing(c) -> match stack with
                | [] | Closing(_)::_ -> Corrupted(a)
                | Opening(d)::_ -> if c = d then parse (List.tl stack) tl else Corrupted(a)


let parse_one_line s =
    s |> Utils.explode |> List.map character_of_string |> parse []

let score_illegal = function
    | OK  -> 0
    | Incomplete(l) -> 0
    | Corrupted(c) -> match c with
        | Closing("()") -> 3
        | Closing("[]") -> 57
        | Closing("{}") -> 1197
        | Closing("<>") -> 25137
        | x -> failwith ("Unexpected illegar char " ^(string_of_character x))

let score_incomplete res =
    let rec loop acc = function
        | [] -> acc
        | c::tl -> 
            let cur = match c with
                | Opening("()") -> 1
                | Opening("[]") -> 2
                | Opening("{}") -> 3
                | Opening("<>") -> 4 
                | _ -> failwith "Unexpected character" in loop (acc*5 + cur) tl in
    match res with
        | OK -> 0
        | Corrupted(c) -> 0
        | Incomplete(l) -> loop 0 l    

let part1 input =
    let results = input |> Utils.read_lines |> List.map parse_one_line in
    results |> List.map score_illegal |> List.fold_left (+) 0 |> Printf.printf "Part 1: %n\n"

let () = part1 "inputs/day10.dat"

let part2 input =
    let results = input |> Utils.read_lines |> List.map parse_one_line in
    let filtered = results |> List.map score_incomplete |> List.filter (fun x -> x <> 0) in
    let sorted = List.sort compare filtered in
    let n = List.length sorted in
    Printf.printf "Part 2: %n\n" (List.nth sorted (n/2))

let () = part2 "inputs/day10.dat"