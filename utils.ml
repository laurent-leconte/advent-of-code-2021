(* to build: ocamlc str.cma utils.ml *)

let read_lines name =
    let ic = open_in name in
    let try_read () =
        try Some (input_line ic) with End_of_file -> None in
    let rec loop acc = match try_read () with
        | Some s -> loop (s :: acc)
        | None -> close_in ic; List.rev acc in
    loop []


(* split according to a given separator *)
let split_by_string sep = Str.split (Str.regexp sep) 

let char_at s i = String.sub s i 1

(* transform a string into a list of chars *)
let explode s = List.init (String.length s) (char_at s) 

let rec transpose = function
   | [] 
   | [] :: _ -> []
   | rows    -> 
       List.map List.hd rows :: transpose (List.map List.tl rows)

let range a b = 
    let start = min a b in
    let stop = max a b in
    List.init (stop - start + 1) (fun x -> start + x)

let acc_str to_string acc x = acc ^ (to_string x) ^ " "

let print_array to_string arr =
     let f = acc_str to_string in
     print_endline @@ Array.fold_left f "" arr

let print_int_array = print_array string_of_int

let print_matrix to_string mat =
    let _ = Array.map (print_array to_string) mat in ()

let print_int_matrix = print_matrix string_of_int