type type_flag = Lit | Op of int

type length = BitLength of int | PacketLength of int

type fields = {
    version: int;
    type_id: type_flag;
    value: int;
    subpackets: packet list}
and packet = Literal of fields | Operator of fields

let bits_of_hexa s =
    let rec loop = function
        | [] -> []
        | hex::tl -> 
            let bits = match hex with
                | "0" -> [0;0;0;0]
                | "1" -> [0;0;0;1]
                | "2" -> [0;0;1;0]
                | "3" -> [0;0;1;1]
                | "4" -> [0;1;0;0]
                | "5" -> [0;1;0;1]
                | "6" -> [0;1;1;0]
                | "7" -> [0;1;1;1]
                | "8" -> [1;0;0;0]
                | "9" -> [1;0;0;1]
                | "A" -> [1;0;1;0]
                | "B" -> [1;0;1;1]
                | "C" -> [1;1;0;0]
                | "D" -> [1;1;0;1]
                | "E" -> [1;1;1;0]
                | "F" -> [1;1;1;1]
                | x -> failwith ("not a hex number: " ^ x) in
            bits@(loop tl) in
    loop @@ Utils.explode s

let int_of_bits b =
    let rec loop pow = function
        | [] -> 0
        | a::tl -> a*pow + loop (2*pow) tl in
    loop 1 (List.rev b)

let opcode = function
    | Lit -> failwith "Literals don't have opcodes"
    | Op n -> n

let take n l = 
    let rec loop hd tl = function
        | 0 -> (List.rev hd, tl)
        | n -> loop ((List.hd tl)::hd) (List.tl tl) (n-1) in
    loop [] l n

let read_version l = 
    let hd, tl = take 3 l in
    (int_of_bits hd, tl)

let read_type l =
    let hd, tl = take 3 l in
    let type_of_list l =
        match (int_of_bits l) with
            | 4 -> Lit
            | n -> Op(n) in
    (type_of_list hd, tl)

let read_length l =
    let tl = List.tl l in
    match List.hd l with
        | 0 -> let (size, rest) = take 15 tl in (BitLength(int_of_bits size), rest)
        | 1 -> let (size, rest) = take 11 tl in (PacketLength(int_of_bits size), rest)
        | a -> failwith ("Bad length type " ^ (string_of_int a))

let string_of_type = function
    | Lit -> "lit"
    | Op n -> "op" ^ (string_of_int n)

let rec string_of_packet = function
    | Literal fields -> Printf.sprintf "v:%d;%s;%d" fields.version (string_of_type fields.type_id) fields.value
    | Operator fields -> let subp = "[" ^ String.concat "|" (List.map string_of_packet fields.subpackets) ^ "]" in
                         Printf.sprintf "v:%d;%s;%s" fields.version (string_of_type fields.type_id) subp

let read_literal l =
    let rec loop acc l = 
        let segment, rest = take 5 l in
        match (take 1 segment) with
            | ([0], tl) -> (int_of_bits (acc@tl), rest)
            | ([1], tl) -> loop (acc@tl) rest 
            | _ -> failwith "Bad segment" in
    loop [] l

let rec parse p =
    let version, rest = read_version p in
    let type_id, rest = read_type rest in
    match type_id with
        | Lit -> 
            let value, rest = read_literal rest in
            (Literal({version; type_id; value; subpackets=[]}), rest)
        | Op _ -> 
            let mode, rest = read_length rest in
            let subpackets, rest = parse_sub rest mode in
            (Operator({version; type_id; subpackets; value=0}), rest)
and parse_sub p = function
    | BitLength l -> 
        if l = 0 then
            ([], p) 
        else
            let packet, rest = parse p in
            let sub_packets, rest' = parse_sub rest (BitLength(l - List.length p + List.length rest)) in
            (packet::sub_packets, rest')
    | PacketLength n ->
        if n = 0 then
            ([], p)
        else
            let packet, rest = parse p in
            let sub_packets, rest' = parse_sub rest (PacketLength(n - 1)) in
            (packet::sub_packets, rest')

let rec sum_version = function
    | Literal f -> f.version
    | Operator f -> f.version + (f.subpackets |> List.map sum_version |> Utils.sum)

let rec evaluate = function
    | Literal f -> f.value
    | Operator f -> 
        let op = opcode f.type_id in 
        match op with
            | 0 -> List.fold_left (+) 0 (f.subpackets |> List.map evaluate)
            | 1 -> List.fold_left ( * ) 1 (f.subpackets |> List.map evaluate)
            | 2 -> List.fold_left min max_int (f.subpackets |> List.map evaluate)
            | 3 -> List.fold_left max min_int (f.subpackets |> List.map evaluate)
            | 5 -> let fst::snd::_ = (f.subpackets |> List.map evaluate) in if (fst > snd) then 1 else 0
            | 6 -> let fst::snd::_ = (f.subpackets |> List.map evaluate) in if (fst < snd) then 1 else 0
            | 7 -> let fst::snd::_ = (f.subpackets |> List.map evaluate) in if (fst = snd) then 1 else 0
            | _ -> failwith "Bad opcode"

(* part 1 *)
let _ = "inputs/day16.dat"
    |> Utils.read_lines
    |> List.map bits_of_hexa
    |> List.map parse
    |> List.map fst
    |> List.map sum_version
    |> List.map string_of_int
    |> List.map print_endline

(* part 2 *)
let _ = "inputs/day16.dat"
    |> Utils.read_lines
    |> List.map bits_of_hexa
    |> List.map parse
    |> List.map fst
    |> List.map evaluate
    |> List.map string_of_int
    |> List.map print_endline