let increase mat = Utils.iterij (fun i j m -> m.(i).(j) <- m.(i).(j) + 1) mat

let neighbors n i j = 
    let within_bounds (i,j) = (i >= 0) && (i < n) && (j >= 0) && (j < n) in
    [(i-1,j-1); (i-1,j); (i-1,j+1);
     (i,j-1);            (i,j+1);
     (i+1,j-1); (i+1,j); (i+1,j+1)]
     |> List.filter within_bounds


let simulate_one_day mat =
    let n, _ = Utils.dim mat in
    increase mat;
    let to_flash = ref [] in
    let init_to_flash i j m = if m.(i).(j) > 9 then to_flash := (i,j)::!to_flash in
    Utils.iterij init_to_flash mat;
    let increase_neighbor (i,j) = 
        let m = mat.(i).(j) in 
        if m > 0 then mat.(i).(j) <- m + 1 else () in
    let rec flash flashed = function
        | [] -> flashed
        | (i,j)::tl ->
            if mat.(i).(j) = 0 then
                flash flashed tl
            else
                (mat.(i).(j) <- 0;
                let neighbors = neighbors n i j in
                List.iter increase_neighbor neighbors;
                let new_to_flash = List.filter (fun (i,j) -> mat.(i).(j) > 9)  neighbors in
                flash (flashed + 1) (tl@new_to_flash)) in
    flash 0 !to_flash


let () = 
    let (mat, m, n) = Utils.read_matrix "inputs/day11.dat" in
    let flashes = ref 0 in
    let first_sync = ref 0 in
    for i=1 to 500 do
        let new_flashes = simulate_one_day mat in
        let () = flashes := !flashes + new_flashes in
        if i=100 then Printf.printf "Part 1: %n\n" !flashes;
        if new_flashes = 100 then Printf.printf "Synced at step %n\n" i;
    done;
    Printf.printf "Part 2: %n\n" !first_sync