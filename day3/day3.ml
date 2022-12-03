let ic = open_in "day3_input" 

let ctoi c = 
  let c = Char.code (c) in
  if c <= 90 then c - 38 else c - 96

let check_first_single l c = 
  let i = ctoi c in
  let r = List.nth l i in
  r := (true, false)

let check_second_single l c = 
  let i = ctoi c in
  let r = List.nth l i in
  r := (fst !r, true)

let check_third_single l c =
  let i = ctoi c in
  let r = List.nth l i in
  if (fst !r) && (snd !r) then (print_char c; r := (false,false) ;i) else 0

let check_first l s =
   String.iter (fun c -> check_first_single l c) s

let check_second l s =
   String.iter (fun c -> check_second_single l c) s

let check_third l s =
   String.fold_left (fun acc c -> acc + (check_third_single l c)) 0 s

let create_init = fun _ -> List.init 53 (fun _ -> ref (false, false))

let rec read l acc idx = 
  let s_line = try input_line ic with End_of_file -> Printf.printf "priority: %d" acc; "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> ()
  | s ->
    let mod3 = idx mod 3 in
    if mod3 = 0 then
      (check_first l s; read l acc (idx + 1))
    else if mod3 = 1 then
      (check_second l s; read l acc (idx + 1))
    else
      (* print_string ("THIRD" ^ (string_of_int idx) ^ "\n"); *)
      read (create_init ()) (acc + check_third l s) (idx + 1)

let _ = read (create_init ()) 0 0