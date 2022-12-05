let ic = open_in "day5_input" 

let re = Str.regexp {|\[\([A-Z]\)\]|} 
let re_move = Str.regexp {|^move \([0-9]+\) from \([0-9]+\) to \([0-9]+\)|}

let parse_single s start =
  try let pos = Str.search_forward re s start in  
  Some (Str.matched_group 1 s, pos)
  with _ -> None

let rec parse_line s start (ll: char list list) =
  match parse_single s start with
  | Some (c, pos) ->
    let idx = pos/4 in
    parse_line s (pos + 4) (List.mapi (fun i l -> if i = idx then l@[c.[0]] else l) ll)
  | None -> if start = 0 then None else Some ll


let parse_move s = 
  if (Str.string_match re_move s 0) then 
    Some (int_of_string (Str.matched_group 1 s), int_of_string (Str.matched_group 2 s)-1, int_of_string (Str.matched_group 3 s)-1) else None

let move size src dst ll =
  let col = List.nth ll src in
  let payload = (List.filteri (fun i _ -> i < size) col) in
  let src_rest = List.filteri (fun i _ -> i >= size) col in 
  List.mapi (fun i l -> if i = src then src_rest else if i = dst then payload@l else l ) ll

let rec read b ll = 
  let s_line = try input_line ic with End_of_file -> 
    let _ = (List.iter (fun l -> print_char (List.hd l)) ll) in
    "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> ()
  | s ->
    if b then 
      match parse_line s 0 ll with
     | Some ll' ->  read b ll'
     | None -> let _ = input_line ic in read false ll
    else
      match parse_move s with
      | Some (size, src, dst) -> read b (move size src dst ll)
      | None -> print_string s


let _ = read true [[];[];[];[];[];[];[];[];[]]
