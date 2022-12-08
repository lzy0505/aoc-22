let ic = open_in "day8_input" 

module IntInt = struct
  type t = int * int
  let compare = compare
end

module Grid = Map.Make(IntInt)

type grid = int Grid.t

let locate x y (g: grid) : int =
  try Grid.find (x,y) g with _ -> -1


let rec scan_l m c x y =
if c < 0 then 0
else
  if (locate c y m) < (locate x y m) then (scan_l m (c-1) x y) +1 
  else 1

let rec scan_t m c x y =
if c < 0 then 0
else
  if (locate x c m) < (locate x y m) then (scan_t m (c-1) x y) +1 
  else 1

let rec scan_r m n c x y =
if c >= n then 0
else
  if (locate c y m) < (locate x y m) then (scan_r m n (c+1) x y) +1 
  else 1


let rec scan_b m n c x y =
if c >= n then 0
else
  if (locate x c m) < (locate x y m) then (scan_b m n (c+1) x y) +1 
  else 1
  
let scan_lt (m: grid) n : grid * grid =
  let rec scan_aux (lg: grid) (tg: grid) x y =
    if x < n && y < n then scan_aux (Grid.add (x,y) (scan_l m (x-1) x y) lg) 
    (Grid.add (x,y) (scan_t m (y-1) x y) tg) (x+1) y
    else if y < n then scan_aux lg tg 0 (y+1)
    else (lg, tg) in
  scan_aux Grid.empty Grid.empty 0 0


let scan_rb (m: grid) n : grid * grid =
  let rec scan_aux (lg: grid) (tg: grid) x y =
    if x < n && y < n then scan_aux (Grid.add (n-1-x,n-1-y) (scan_r m n (n-x) (n-1-x) (n-1-y)) lg) 
    (Grid.add (n-1-x,n-1-y) (scan_b m n (n-y) (n-1-x) (n-1-y)) tg) (x+1) y
    else if y < n then scan_aux lg tg 0 (y+1)
    else (lg, tg) in
  scan_aux Grid.empty Grid.empty 0 0

let check lg tg rg bg x y =
  Printf.printf "%d*%d*%d*%d"
   (locate x y lg)
  (locate x y rg)
   (locate x y tg)
   (locate x y bg) 
  ; print_string "/" ;locate x y lg *
    locate x y rg *
    locate x y tg *
   locate x y bg 

let scan m n =
  let lg, tg = scan_lt m n in
  let rg, bg = scan_rb m n in
  let rec vis x y acc =
    if x < n && y < n then vis (x+1) y (max (check lg tg rg bg x y) acc)
    else if y < n then (print_string "\n";vis 0 (y+1) acc)
    else acc in
  vis 0 0 0

let rec scan_row s m col row  =
    if row < String.length s then scan_row s (Grid.add (row,col) ((int_of_char (s.[row]))-48) m) col (row+1)
    else m

let rec read col m = 
  let s_line = try input_line ic with End_of_file -> 
    let _ = print_int (scan m col) in
    "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> print_string "END"
  | s ->
    let m' = scan_row s m col 0 in
    read (col+1) m'


let _ = read 0 Grid.empty