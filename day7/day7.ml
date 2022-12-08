let ic = open_in "day7_input" 

type entry =
| File of string * int
| Dir of string * (entry list ref)

type cmd = 
| CD_down of string
| CD_up
| LS

let root = ref [Dir ("/", ref [])]


let re_cmd = Str.regexp {|^\$ \(cd \(.+\)\|ls\)|} 

let is_cmd s = 
  if (Str.string_match re_cmd s 0) then 
    let s' = Str.matched_group 1 s in
    if s' = "ls" then (print_string "LS\n"; Some LS)
    else  
      let dir = Str.matched_group 2 s in
      if dir = ".." then (print_string "CD ..\n"; Some CD_up)
      else (Printf.printf "CD %s\n" dir; Some (CD_down dir))
else (print_string ("NONE");None)

let re_entry = Str.regexp {|^\([0-9]+\|dir\) \([a-z\.]+\)|}

let is_entry s =
  let _ = (Str.string_match re_entry s 0) in 
    let ty = Str.matched_group 1 s in
    let name = Str.matched_group 2 s in
    if ty = "dir" then 
      (Printf.printf "DIR: %s\n" name;
      Dir (name, ref []))
    else(Printf.printf "FILE: %s\n" name; File (name, int_of_string ty))
  

  let rec traverse e acc =
    match e with
    | File (name, size) -> size, acc
    | Dir (name, rl) -> 
      let size, acc = List.fold_left (fun (size_acc, acc_acc) e -> match e with
      | File _ -> fst (traverse e acc_acc) + size_acc, acc_acc
      | Dir _ -> let sz, ac = traverse e acc_acc in sz + size_acc, (
           sz :: ac
      )) (0,acc) (!rl) in
      Printf.printf "DIR %s SIZE %d\n" name size; size, acc


let rec read path cwd = 
  let s_line = try input_line ic with End_of_file -> 
    let size, l =  (traverse (List.hd (!root)) []) in
    let need = size - 40000000 in
    let l' = List.fast_sort compare l in
    let ans = List.hd (List.filter (fun z -> z >= need) l') in
    print_int ans;
    "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> print_string "END"
  | s ->
    match is_cmd s with
    | Some LS -> read path cwd
    | Some CD_up -> read (List.tl path) (List.hd path)
    | Some (CD_down s) -> 
      let new_cwd = (List.fold_left (fun acc e -> match e with
      | Dir (s', r) -> if s' = s then r else acc
      | _ -> acc) (ref []) (!cwd)) in
      read (cwd::path) new_cwd
    | None -> let e = is_entry s in
      cwd := e::(!cwd);
      read path cwd


let _ = read [] root