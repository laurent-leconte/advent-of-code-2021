let example = [
    "NNCB";
    "";
    "CH -> B";
    "HH -> N";
    "CB -> H";
    "NH -> C";
    "HB -> C";
    "HC -> B";
    "HN -> C";
    "NN -> C";
    "BH -> H";
    "NC -> B";
    "NB -> B";
    "BN -> B";
    "BB -> N";
    "BC -> B";
    "CC -> N";
    "CN -> C"
]

let parse_input lines =
    let template::_::formulas = lines in
    let instructions = Hashtbl.create (List.length formulas) in
    let add_instruction table s =
        let input::output::_ = Utils.split_by_string " -> " s in
        Hashtbl.add table input output;
        table in
    (Utils.explode template, List.fold_left add_instruction instructions formulas)

let substitute n (template, instructions) =
    let rec do_one acc = function
        | [] -> List.rev acc
        | [a] -> List.rev (a::acc)
        | a::(b::_ as tl) -> match Hashtbl.find_opt instructions (a ^b) with
            | None -> do_one (a::acc) tl
            | Some c -> do_one (c::a::acc) tl in
    let rec loop acc = function
        | 0 -> acc
        | n -> let next = do_one [] acc in loop next (n-1) in
    loop template n

let count_occurrences l = 
    let counter = Hashtbl.create 26 in
    let rec loop = function
        | [] -> counter
        | a::tl -> 
            match Hashtbl.find_opt counter a with
                | None -> Hashtbl.add counter a 1; loop tl
                | Some n -> Hashtbl.replace counter a (n+1); loop tl in
    loop l

let puzzle n input =
    let counter = input
        |> parse_input
        |> substitute n 
        |> count_occurrences in
    let sorted = List.sort compare (Hashtbl.fold (fun a b l -> (b,a)::l) counter []) in
    let (min_count, min_val) = List.hd sorted in
    let (max_count, max_val) = List.hd @@ List.rev sorted in
    Printf.printf "Most common is %s (%n), least common is %s (%n), delta is %n\n" max_val max_count min_val min_count (max_count - min_count)

let () = puzzle 10 @@ Utils.read_lines "inputs/day14.dat"

let () = puzzle 40 example