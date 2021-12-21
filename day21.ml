(* state : position 1 position 2 score 1 score 2 *)
type state = int * int * int * int

let exercice_die n = (n mod 100) + 1

let next_roll die step =
  let die_value = exercice_die die
  in (die_value, step + 1)

let roll_three die step =
    let roll1 = exercice_die die in
    let roll2 = exercice_die roll1 in
    let roll3 = exercice_die roll2 in
    (roll1 + roll2 + roll3, roll3, step + 3)


let game1 start1 start2 =
    let rec next_round die step pos1 pos2 score1 score2 =
        Printf.printf "Step %d\nPlayer 1: %d %d\nPlayer 2: %d %d\n" step pos1 score1 pos2 score2;
        (* player 1 *)
        let rolls, die, step = roll_three die step in
        let pos1 = (pos1 + rolls - 1) mod 10 + 1 in
        let score1 = score1 + pos1 in
        if score1 >= 1000 then 
            score2*step 
        else
            (* player 2 *)
            let rolls, die, step = roll_three die step in
            let pos2 = (pos2 + rolls - 1) mod 10 + 1 in
            let score2 = score2 + pos2 in
            if score2 >= 1000 then 
                score1*step 
            else
                next_round die step pos1 pos2 score1 score2 in
    next_round 0 0 start1 start2 0 0

let () = Printf.printf "Part1: %d\n" (game1 6 8)


let addt (a,b) (c,d) = (a+c, b+d)
let scalar (a,b) c = (a*c, b*c)
let maxt (a,b) = max a b
let roll_odds = [(3,1); (4,3); (5,6); (6,7); (7,6); (8,3) ;(9,1)]

let update_state roll is_p1 state =
    let (pos1, pos2, score1, score2) = state in
    if is_p1 then
        let new_pos1 = (pos1 + roll - 1) mod 10 + 1 in 
        (new_pos1, pos2, score1 + new_pos1, score2)
    else
        let new_pos2 = (pos2 + roll - 1) mod 10 + 1 in 
        (pos1, new_pos2, score1, score2 + new_pos2)

let rec game2 self is_p1 state =
    let do_one_roll roll =
        let (p1, p2, s1, s2) = update_state roll is_p1 state in
        if is_p1 && s1 >= 21 then (1,0)
        else if Bool.not is_p1 && s2 >= 21 then (0,1)
        else self (Bool.not is_p1) (p1, p2, s1, s2) in
    let weighted_rolls = List.map (fun (roll, odds) -> scalar (do_one_roll roll) odds) roll_odds in
    List.fold_left addt (0,0) weighted_rolls

let rec memoized_game2 = Utils.memo_rec game2

let () = Printf.printf "Part2: %d\n" (maxt (memoized_game2 true (6,8,0,0)))