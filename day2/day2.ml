let ic = open_in "day2_input" 

type shape = 
| Scissor
| Paper
| Rock

let shape_score = function
| Scissor -> 3
| Paper -> 2
| Rock -> 1

let win_score = function
| Scissor, Paper -> 0
| Scissor, Rock -> 6
| Rock, Paper -> 6
| Rock, Scissor -> 0
| Paper, Rock -> 0
| Paper, Scissor -> 6
| _ -> 3

let score opp me = 
  win_score (opp,me) + shape_score me

let translate s =
  if s = 'A' then Rock
  else if s = 'B' then Paper
  else Scissor

type s = 
| Win
| Draw
| Lose

let translate_s s =
  if s = 'X' then Lose
  else if s = 'Y' then Draw
  else Win

let me = function
| Scissor, Win -> Rock
| Scissor, Lose -> Paper
| Rock, Win -> Paper
| Rock, Lose -> Scissor
| Paper, Win -> Scissor
| Paper, Lose -> Rock
| _ as sha, Draw -> sha

let rec read acc = 
  let s_line = try input_line ic with End_of_file -> Printf.printf "score: %d" acc; "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> ()
  | s ->
    let opp = translate (String.get s 0) in
    let me = me (opp,(translate_s (String.get s 2))) in
    read (acc + (score opp me))

let _ = read 0 