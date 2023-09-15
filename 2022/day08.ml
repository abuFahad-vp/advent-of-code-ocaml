open Containers

let input = "inputs/day8.txt" |> open_in |> IO.read_lines_l 
let grid =  Array.make (List.length input) [|0|];;

let char_to_int c = int_of_char c - int_of_char '0';;

List.iteri (fun i b -> 
  let row = b |> String.to_seq |> Array.of_seq |> Array.map (char_to_int) in
  grid.(i) <- row) input;;

let len = Array.length grid;;

let rec traverse x_fun y_fun fun_apply coord cell acc m n grd =
  let x, y = x_fun (fst coord), y_fun (snd coord) in
  if x <= m - 1 && x >= 0 && y >= 0 && y <= n - 1 then
    let new_acc = fun_apply acc cell (x, y, grd.(x).(y)) in
    traverse x_fun y_fun fun_apply (x, y) cell new_acc m n grd
  else
    acc

let thd a = match a with (_,_,a) -> a;;

let isvisible acc ele com = 
  if thd ele <= thd com then false else
  if not acc then false else true

let isgood acc ele com =
  if thd ele > thd com && fst acc then (fst acc, (snd acc) + 1) else
  if thd ele <= thd com && fst acc then (false, (snd acc) +  1) else
    (fst acc, snd acc);;

let traverse_dir i k _fun acc dir = 
  match dir with 
  | "up" -> traverse (fun x -> x - 1) (fun y -> y) _fun    (i,k) (i,k,grid.(i).(k)) acc len len grid
  | "down" -> traverse (fun x -> x + 1) (fun y -> y) _fun  (i,k) (i,k,grid.(i).(k)) acc len len grid
  | "right" -> traverse (fun x -> x) (fun y -> y + 1) _fun (i,k) (i,k,grid.(i).(k)) acc len len grid
  | "left" -> traverse (fun x -> x) (fun y -> y - 1) _fun  (i,k) (i,k,grid.(i).(k)) acc len len grid
  | _ -> acc;;

let part1 = ref 0;;
let part2 = ref 0;;

for i=1 to len - 2 do
  for k=1 to len - 2 do
    if List.exists (fun x -> x) (List.map 
    (traverse_dir i k isvisible true) ["up";"down";"left";"right"]) then incr part1;

    part2 := max !part2 (List.fold_left (fun a b -> a * (snd b) ) 1 (List.map 
    (traverse_dir i k isgood (true,0)) ["up";"down";"left";"right"]));
  done
done;;
Printf.printf "part 1 = %d, part 2 = %d\n" (!part1 + (4 * len - 4)) !part2

(*
let print_grid grid = 
  for i=0 to len - 1 do
    for k=0 to len - 1 do
      Printf.printf "%d" grid.(i).(k)
    done;
    print_endline "";
  done;;
*)
