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