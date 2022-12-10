let ic = open_in "day9_input" 

type pos = int * int

type hor = L | R | HN
type ver = U | D | VN

let hor_minus_p h1 h2 =
match h1, h2 with
| L, R -> HN
| R, L -> HN
| _ , HN -> h1
| HN , _ -> h2
| _ when h1 = h2 -> h1

let ver_minus_p v1 v2 =
match v1, v2 with
| U, D -> VN
| D, U -> VN
| _ , VN -> v1
| VN , _ -> v2
| _ when v1 = v2 -> v1


let hor_minus_d h1 h2 =
match h1, h2 with
| _ when h1 = h2 -> h1
| _ -> HN

let ver_minus_d v1 v2 =
match v1, v2 with
| _ when v1 = v2 -> v1
| _ -> VN


let p_minus (rp_h, rp_v) (d_h,d_v) =
  if rp_h = d_h && not (rp_h = HN) then 
    rp_h, ver_minus_d rp_v d_v
  else if rp_v = d_v && not (rp_v = VN)then 
    hor_minus_d rp_h d_h, rp_v
  else
  hor_minus_p rp_h d_h, ver_minus_p rp_v d_v
  
  
let d_minus (rp_h, rp_v) (d_h,d_v) = 
if rp_h = HN && rp_v = VN || d_h = HN && d_v = VN then HN, VN else
  if rp_h = d_h then 
    rp_h, ver_minus_p rp_v d_v
  else if rp_v = d_v then 
   hor_minus_p rp_h d_h, rp_v
  else 
  hor_minus_d rp_h d_h, ver_minus_d rp_v d_v

type relpos = hor * ver
type dir = hor * ver

let mh (x,y) h = 
  match h with
  | L -> x-1,y
  | R -> x+1,y
  | HN -> x,y

let mv (x,y) v = 
  match v with
  | U -> x,y+1
  | D -> x,y-1
  | VN -> x,y

let m p (h,v) = mh (mv p v) h

let p_hor = function  
  | L -> "L"
  | R -> "R"
  | HN -> "_"

let p_ver = function  
  | U -> "U"
  | D -> "D"
  | VN -> "_"

let pr (h,v) = p_hor h ^ p_ver v

let move p rp d : pos * relpos * dir =
  let rp' = p_minus rp d in
  let d' = d_minus rp d in
  (Printf.printf "%s - %s = %s, %s \n" (pr rp) (pr d) (pr rp') (pr d');
  m p d', rp', d')

let parse_dir c =
 if c = 'R' then (R, VN)
 else if c = 'L' then (L, VN)
 else if c = 'U' then (HN, U)
 else (HN, D)


let move_rope rope d visited =
  print_string "ROPE\n";
  let rope', _ = List.fold_left (fun (new_rope,d) (p,rp) -> 
    let p', rp', d' = move p rp d in
    (p',rp') :: new_rope, d' 
    ) ([],d) rope in
    let tp =  (fst (List.hd rope')) in
   List.rev rope',(if not (List.mem tp visited) then tp :: visited else visited)
  

let rec read visited rope= 
  let s_line = try input_line ic with End_of_file -> 
    print_int (List.length visited);
    "<EOF>" in
  match s_line with
  | s when s = "<EOF>" -> print_string "END"
  | s -> 
    let d = parse_dir s.[0] in 
    let n = int_of_string (String.sub s 2 (String.length s - 2)) in
    let _ = Printf.printf "====%s====\n" s in
    let (rope, visited) = List.fold_left (fun (rope,visited) _ -> 
        move_rope rope d visited
      ) (rope,visited) (List.init n (fun _ -> ())) in
    read visited rope

let _ = read [(0,0)] (List.init 9 (fun _ -> (0,0), (HN,VN)))