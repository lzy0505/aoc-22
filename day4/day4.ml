let ic = open_in "day4_input" 

let get_range s =
  let sl = String.split_on_char '-' s in
  let left, right = List.hd sl, List.hd (List.tl sl) in
  (int_of_string left), (int_of_string right)

let check_overlapped (l1, r1) (l2, r2) =
  l2 <= r1 && l1 <= r2 

let rec read acc = 
  let s_line = try input_line ic with End_of_file -> Printf.printf "contained: %d" acc; "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> ()
  | s ->
    let sl = String.split_on_char ',' s in
    let left, right = List.hd sl, List.hd (List.tl sl) in
    let left_r, right_r = get_range left, get_range right in
    if (check_overlapped left_r right_r) || (check_overlapped right_r left_r) 
      then read (acc + 1) else read acc

let _ = read 0
