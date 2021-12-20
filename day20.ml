let pixel = function
        | "." -> 0
        | "#" -> 1
        | _   -> failwith "invalid pixel"

let load_iea s =
    let iea = Array.make 512 0 in
    let f i c = iea.(i) <- pixel c in
    List.iteri f (Utils.explode s);
    iea

let load_image lines =
    let n = List.length lines in
    let m = String.length @@ List.hd lines in
    let image = Array.make_matrix n m 0 in
    let write_line i s =
        let line = List.map pixel (Utils.explode s) in
        let write_one j d = image.(i).(j) <- d in
        List.iteri write_one line in
    List.iteri write_line lines;
    image

let parse_input input = 
    let iea = load_iea (List.hd input) in
    let image = load_image (List.tl (List.tl input)) in
    (iea, image)

let enhance_once (iea, image) iter =
    let m, n = Utils.dim image in
    let score_pixel i j =
        let safe_get i j =
            if i < 0 || j < 0 || i >= m || j >= n then (iter mod 2) else image.(i).(j) in
        let neighbors = [(i-1,j-1); (i-1,j); (i-1,j+1); (i,j-1); (i,j); (i,j+1); (i+1,j-1); (i+1,j); (i+1,j+1)] in
        let poly acc x = 2*acc + x in
        let neighbor_vals = List.map (fun (i,j) -> safe_get i j) neighbors in
        (* print_endline @@ String.concat "," (List.map string_of_int neighbor_vals); *)
        let score = List.fold_left poly 0 neighbor_vals in
        (* Printf.printf "score %d %d: %n\n" i j score; *)
        iea.(score) in
    let new_image = Array.make_matrix (m+2) (n+2) 0 in
    let update i j _ = new_image.(i).(j) <- score_pixel (i-1) (j-1) in
    Utils.iterij update new_image;
    (iea,new_image)

let rec enhance n (iea, image) =
    if n = 0 then image else enhance (n-1) (enhance_once (iea, image) n)

let count_pixels image = Utils.foldij (fun i j acc p -> acc + p) 0 image

let () = "inputs/day20.dat"
    |> Utils.read_lines
    |> parse_input
    |> enhance 50
    |> count_pixels
    |> Printf.printf "%n\n"