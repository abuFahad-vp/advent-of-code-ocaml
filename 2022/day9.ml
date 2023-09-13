type my_tuple = int * int

module MyTupleSet = Set.Make(struct
  type t = my_tuple
  let compare = compare
end)

let ic = open_in "inputs/day9.txt"
let cell = ref (0,0)
let head_moves = ref [(0,0)]

let rec incr_pos cell dir n =
  if n > 0 then
    match dir with
    | "R" -> let c = ((fst cell) + 1, snd cell)  in head_moves := !head_moves @ [c];incr_pos c dir (n - 1)  
    | "L" -> let c = ((fst cell) - 1, snd cell)  in head_moves := !head_moves @ [c];incr_pos c dir (n - 1)  
    | "U" -> let c = (fst cell , (snd cell) + 1) in head_moves := !head_moves @ [c];incr_pos c dir (n - 1)  
    | "D" -> let c = (fst cell , (snd cell) - 1) in head_moves := !head_moves @ [c];incr_pos c dir (n - 1)  
    | _ -> cell
  else
    cell

let rec read ic = 
  try
    let line = input_line ic in
    let tokens = String.split_on_char ' ' line in
    let dir = List.hd tokens in
    let n = int_of_string (List.hd (List.tl tokens)) in
    cell := incr_pos !cell dir n;
    read ic
  with _ -> ();;

read ic;;

let follow_up hx hy tx ty = 
  if abs (hx - tx) <= 1 && abs (hy - ty) <= 1 then (tx, ty)
  else (
    let dx = compare (hx - tx) 0 in
    let dy = compare (hy - ty) 0 in
    (tx + dx, ty + dy))

let tail_moves head_moves = 
  let rec aux head_moves acc tl_x tl_y = 
    match head_moves with
    | [] -> acc
    | (x,y)::t ->
      let (xt, yt1) = follow_up x y tl_x tl_y in
      aux t (acc @ [(xt, yt1)]) xt yt1 
  in
  aux head_moves [] 0 0;;

let rec apply_tale_moves moves n = 
  if n <= 0 then MyTupleSet.cardinal (MyTupleSet.of_list moves) else
  apply_tale_moves (tail_moves moves) (n - 1);;

Printf.printf "part 1 = %d, part 2 = %d\n" (apply_tale_moves !head_moves 1) (apply_tale_moves !head_moves 9);;
