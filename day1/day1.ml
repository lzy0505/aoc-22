let ic = open_in "day1_input" 


let compare (l: int list) (i : int) =
  if List.length l < 3 then List.sort (compare) (i :: l) else
  if List.hd l < i then List.sort compare (i :: List.tl l)
  else l


let rec read curr_max_l curr_acc = 
  let s_line = try input_line ic with End_of_file -> Printf.printf "max: %d" (List.fold_left (fun i acc -> i + acc) 0 curr_max_l); "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> 0
  | s when s = "" ->
    read (compare curr_max_l curr_acc) 0
  | s ->
    let i = try int_of_string s with _ -> print_string s_line;0 in
    read curr_max_l (curr_acc + i)

let _ = read [] 0 