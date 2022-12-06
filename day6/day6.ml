let ic = open_in "day6_input" 

let rec scan idx window s =
  try
    let c = s.[idx] in
    let (b, idx') = (List.fold_left (fun (b, i) c'-> if b then b, i else if c' = c then true,i else b, i+1) (false,0) window) in
    if not b then  if List.length window = 13 then idx + 1 else scan (idx + 1) (window @[c]) s
    else scan (idx + 1) ((List.filteri (fun i _ -> i > idx') window) @[c]) s 
  with _ -> idx 

let rec read i = 
  let s_line = try input_line ic with End_of_file -> 
    let _ = print_int i in
    "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> print_string "END"
  | s ->
    print_string "START\n";
    read (scan 0 [] s)


let _ = read (-1)