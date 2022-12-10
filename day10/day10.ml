
let ic = open_in "day10_input" 

let draw cycle reg buf = 
      if cycle mod 40 = 0 then 
          (print_string (buf ^ "\n"); "")
    else
(* Printf.printf "%d, %s\n" reg buf; *)
  if (reg-1) <= String.length buf && String.length buf <= (reg +1) then buf^"#" else buf ^ "."

let rec read cycle reg buf = 
  let s_line = try input_line ic with End_of_file -> 
    "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> print_string "END"
  | s -> 
    if s = "noop" then 
          read (cycle + 1) reg (draw cycle reg buf)
    else 
      let s = String.sub s 5 (String.length s - 5) in
      let n = int_of_string s in
      read (cycle+2) (reg + n) (draw (cycle+1) reg (draw cycle reg buf))

let _ = read 1 1 ""