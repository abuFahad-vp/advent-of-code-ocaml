open Containers
type cell = {
  p : int;
  v : int;
}

let input = 
  "inputs/day12.txt" |> open_in |> IO.read_lines_l 
  |> List.map (fun line -> line |> String.to_list 
  |> List.map (fun x -> if 83 = (int_of_char x) then 96 else if (int_of_char x) = 69 then 123 else (int_of_char x))
  |> Array.of_list) |> Array.of_list;;

let m = Array.length (input);;
let n = Array.length (input.(0));;

let isallowed (x,y) = if 0 <= x && x < m && 0 <= y && y < n then true else false;;

let count = ref 0;;

let graph = Array.make (m * n) (0, [{p=0;v=3}; {p=0;v=4}]);;
let start = ref 0;;
let starts = ref [];;
let stop = ref 0;;

for i=0 to m - 1 do
  let branches = ref [] in
  for k=0 to n - 1 do
    branches := [];
    let current = input.(i).(k) in
    if current = 123 then (stop := !count;);
    if current = 96 then (start := !count;);
    if current = 97 || current = 96 then (starts := !count :: !starts);

    if (isallowed (i, k + 1)) && (current - input.(i).(k + 1)) >= -1 then (
      branches := {p = !count; v = !count + 1} :: !branches);

    if (isallowed (i, k - 1)) && (current - input.(i).(k - 1)) >= -1 then (
      branches := {p = !count; v = !count - 1} :: !branches);

    if (isallowed (i + 1, k)) && (current - input.(i + 1).(k)) >= -1 then (
      branches := {p = !count; v = !count + n} :: !branches);

    if (isallowed (i - 1, k)) && (current - input.(i - 1).(k)) >= -1 then (
      branches := {p = !count; v = !count - n} :: !branches);
      
    graph.(!count) <- (!count, !branches);
    incr count
  done;
done

let bfs_r graph start stop = 
  let q = Queue.create () in
  Queue.push start q;
  let vs = [start.v] in
  let rec bfs q vs vs_so_far = 
    if not (Queue.length q = 0) then
      begin
        let current = Queue.pop q in
        if current.v = stop then vs_so_far else 
        let vs' = 
          List.fold_left (fun acc x ->
            if List.mem x.v vs then acc else
              (Queue.push x q; x.v :: acc)) [] (snd graph.(current.v))
        in
        bfs q (List.append vs' vs) (current::vs_so_far) 
      end
    else
      []
  in
  bfs q vs [] ;;

let node_length lst end_p = 
  if List.length lst = 0 then max_int else
  let start = List.hd lst in
  let rec nol' lst p count = 
    let v = ref (-1) in
    List.iter (fun x -> if x.v = p then v := x.p) lst;
    if !v = end_p then count else nol' lst !v (count + 1)
  in
  nol' lst start.p 2
;;

let part1 = ref 0;;

let part2 = List.fold_left (fun acc x -> 
  let pos = node_length (bfs_r graph {p=x;v=x} !stop) x in
  if x = !start then (part1 := pos);
  min acc pos) max_int !starts;;

Printf.printf "part 1 = %d, part 2 = %d\n" !part1 part2;;
