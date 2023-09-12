let ic = open_in "inputs/day9.txt"
let cell = ref (0,0)
let head_moves = ref [(0,0)]

type my_tuple = int * int

module MyTupleSet = Set.Make(struct
  type t = my_tuple
  let compare = compare
end)

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

let what_to_incr xh yh xt yt = 
  let d1 = xh - xt in
  let d2 = yh - yt in

  if d1 = 2 && d2 = 1 || d1 = 1 && d2 = 2 then (xt + 1, yt + 1) else

  if d1 = 2 && d2 = -1 || d1 = 1 && d2 = -2 then (xt + 1, yt - 1) else

  if d1 = -2 && d2 = 1 || d1 = -1 && d2 = 2 then (xt - 1, yt + 1) else

  if d1 = -2 && d2 = -1 || d1 = -1 && d2 = -2 then (xt - 1, yt - 1) else
  
  ((xt + (d1 / 2)), yt + (d2 / 2));;

let tail_moves head_moves = 
  let rec aux head_moves acc tl_x tl_y = 
    match head_moves with
    | [] -> acc
    | (x,y)::t ->
      let (xt, yt1) = what_to_incr x y tl_x tl_y in
      aux t (acc @ [(xt, yt1)]) xt yt1 
  in
  aux head_moves [] 0 0;;

let tl_moves = !head_moves |> tail_moves |> tail_moves |> tail_moves |> tail_moves |> tail_moves
    |> tail_moves |> tail_moves |> tail_moves |> tail_moves;;

let move_set = MyTupleSet.of_list tl_moves;;

Printf.printf "%d\n" (MyTupleSet.cardinal move_set);;
