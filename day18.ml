type num =
    | Leaf of int
    | Pair of num * num

type token = 
    | Open
    | Close
    | Sep
    | Lit of int

let token_of_char = function
    | '[' -> Open
    | ']' -> Close
    | ',' -> Sep
    | '0'..'9' as a -> Lit (int_of_char a - int_of_char '0')
    | _ -> failwith "unexpected character"

let num_of_string s =
    let rec make s idx =
        match token_of_char (s.[idx]) with
            | Lit n -> (Leaf n, idx + 1)
            | Open -> 
                let (t, idx) = make s (idx + 1) in
                let (u, idx) = make s (idx + 1) in
                (Pair (t,u), idx)
            | Close | Sep -> make s (idx + 1) in
    fst (make s 0)

let rec string_of_num = function
    | Leaf n -> string_of_int n
    | Pair (t,u) -> "[" ^ string_of_num t ^ "," ^ string_of_num u ^ "]"

let rec split = function
    | Leaf n -> 
        if n >= 10 then (Pair(Leaf(n/2), Leaf(n - n/2)), true) else (Leaf(n), false)
    | Pair (t,u) ->
        let t1, s1 = split t in
        if s1 then 
            (Pair(t1, u), true) 
        else
            let t2, s2 = split u in
            if s2 then
                (Pair(t, t2), true)
            else
                (Pair(t, u), false)

let rec update_leftmost a = function
    | Leaf n ->Leaf (n+a)
    | Pair (t,u) -> Pair (update_leftmost a t, u)

let rec update_rightmost a = function
    | Leaf n ->Leaf (n+a)
    | Pair (t,u) -> Pair (t, update_rightmost a u)

let rec explode depth = function
    | Leaf n -> (Leaf(n), false, 0, 0)
    | Pair (Leaf t, Leaf u) as p ->
        if depth >= 4 then 
            (Leaf(0), true, t, u)
        else 
            (p, false, 0, 0)
    | (Pair (t, u) as p) -> 
        let t', exploded, left, right = explode (depth + 1) t in
        if exploded then
            let u' = (if right > 0 then update_leftmost right u else u) in
            (Pair(t', u'), true, left, 0)
        else
            let u', exploded, left, right = explode (depth + 1) u in
            if exploded then
                let t' = (if left > 0 then update_rightmost left t else t) in
                (Pair(t', u'), true, 0, right)
            else 
                (p, false, 0, 0)

let rec reduce n =
    let n', exploded, _, _ = explode 0 n in
    if exploded then 
        reduce n'
    else
        let n', was_split = split n in
        if was_split then
            reduce n'
    else n'

let rec magnitude = function
    | Leaf n -> n
    | Pair (t, u) -> 3*(magnitude t) + 2*(magnitude u)

let add n m = reduce @@ Pair(n, m)

let example = [
    "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]";
    "[[[5,[2,8]],4],[5,[[9,9],0]]]";
    "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]";
    "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]";
    "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]";
    "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]";
    "[[[[5,4],[7,7]],8],[[8,3],8]]";
    "[[9,3],[[9,9],[6,[4,9]]]]";
    "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]";
    "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
]

let part1 input =
    let numbers = List.map num_of_string input in
    List.fold_left add (List.hd numbers) (List.tl numbers) |> magnitude

let rec combination = function
    | [] | _::[]-> []
    | a::tl -> (List.map (fun x -> [a;x]) tl)@(combination tl)

let part2 input =
    let combinations =combination (List.map num_of_string input) in 
    let totals = List.map (fun [a;b] -> magnitude @@ add a b) combinations in 
    List.hd @@ List.rev @@ List.sort compare totals

let () = "inputs/day18.dat" |> Utils.read_lines |> part1 |> string_of_int |> print_endline
let () = "inputs/day18.dat" |> Utils.read_lines |> part2 |> string_of_int |> print_endline