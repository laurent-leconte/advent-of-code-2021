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

let add_to_counter table key amount =
    match Hashtbl.find_opt table key with
        | None -> Hashtbl.add table key amount; table
        | Some n -> Hashtbl.replace table key (n + amount); table

let bigrams s =
    let n = String.length s in
    let bigram pos = String.sub s pos 2 in
    Utils.range 0 (n - 2) |> List.map bigram

let parse_input lines =
    let template::_::formulas = lines in
    let instructions = Hashtbl.create (List.length formulas) in
    let add_instruction table s =
        let input::output::_ = Utils.split_by_string " -> " s in
        let outputs = [(String.sub input 0 1) ^ output; output ^ (String.sub input 1 1)] in
        Hashtbl.add table input outputs;
        table in
    (template, List.fold_left add_instruction instructions formulas)

let init_counter bigrams =
    let counter = Hashtbl.create 10 in
    let increment table x = add_to_counter table x 1 in
    List.fold_left increment counter bigrams

(* returns a new counter by "expanding" each key of the input counter *)
let expand n instructions counter =
    let one_round counter =
        let new_counter = Hashtbl.create (Hashtbl.length counter) in
        (* takes one key/value from the input counter and increases the 
        two entries defined by the instructions in the new counter, then returns
        the updated counter *)
        let expand_one key value new_counter =
            let to_update = Hashtbl.find instructions key in
            let increase_by t k = add_to_counter t k value in
            List.fold_left increase_by new_counter to_update in
        Hashtbl.fold expand_one counter new_counter in
    let rec loop acc = function
        | 0 -> acc
        | n -> loop (one_round acc) (n - 1) in
    loop counter n

let count_occurrences bigrams = 
    let counter = Hashtbl.create 26 in
    let count_bigram key value table =
        let increase_by t k = add_to_counter t k value in
        List.fold_left increase_by table (Utils.explode key) in
    Hashtbl.fold count_bigram bigrams counter

let puzzle n input =
    let (template, instructions) = parse_input input in
    let occurrences = template
        |> bigrams
        |> init_counter
        |> expand n instructions 
        |> count_occurrences in
    let round_up i = if i mod 2 == 0 then i/2 else (i+1)/2 in
    let sorted = List.sort compare (Hashtbl.fold (fun a b l -> (round_up b,a)::l) occurrences []) in
    let (min_count, min_val) = List.hd sorted in
    let (max_count, max_val) = List.hd @@ List.rev sorted in
    Printf.printf "Most common is %s (%n), least common is %s (%n), delta is %n\n" max_val max_count min_val min_count (max_count - min_count)

let () = puzzle 10 (Utils.read_lines "inputs/day14.dat")
let () = puzzle 40 (Utils.read_lines "inputs/day14.dat")
