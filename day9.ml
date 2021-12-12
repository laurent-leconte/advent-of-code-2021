let parse_input data =
    let n = List.length data in
    let m = String.length @@ List.hd data in
    let ground = Array.make_matrix (n+2) (m+2) 10 in
    let write_line i s =
        let line = List.map int_of_string (Utils.explode s) in
        let write_one j d = ground.(i+1).(j+1) <- d in
        List.iteri write_one line in
    List.iteri write_line data;
    (ground, n, m)

let low_points (ground, n, m) =
    let is_low i j =
        let here = ground.(i).(j) in
        here < ground.(i-1).(j) &&
        here < ground.(i+1).(j) &&
        here < ground.(i).(j-1) &&
        here < ground.(i).(j+1) in
    let result = ref [] in
    for i = 1 to n+1 do
        for j = 1 to m+1 do
            if is_low i j then result := (i, j)::!result else ()
        done;
    done;
    !result
    
let risk ground (x,y) = ground.(x).(y) + 1

let eligible ground (x,y) = 
    let g = ground.(x).(y) in (g >= 0) && (g < 9) 

let neighbors i j = [(i+1,j);(i-1,j);(i,j-1);(i,j+1)]

(* Recursively fill a basin, starting from any point within it.
For each point, mark it as visited, add 1 to the running total, and add its eligible neighbors to the queue.
A neighbor is eligible if it's not a peak (<> 9) or a border (<> 10) and it's not yet visited (<> -1) *)
let rec fill_basin ground c = function
    | [] -> c
    | (i,j)::tl -> 
        if ground.(i).(j) == -1 then 
            fill_basin ground c tl 
        else
            let () = ground.(i).(j) <- -1 in
            let to_add = List.filter (eligible ground) (neighbors i j) in
            fill_basin ground (c + 1) (tl@to_add)

let wrap i = [i]

let () =
    let ground, n, m = "inputs/day9.dat"
        |> Utils.read_lines
        |> parse_input in
    let low_points = low_points (ground, n, m) in
    (* part 1 *)
    low_points |> List.map (risk ground) |> List.fold_left (+) 0 |> Printf.printf "First part: %n\n";
    (* part 2 *)
    let a::b::c::_ = low_points |> List.map wrap |> List.map (fill_basin ground 0) |> List.sort compare |> List.rev in
    Printf.printf "Second part: %n\n" (a*b*c);
