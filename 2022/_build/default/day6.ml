open Containers

module CharSet = struct
  include Set.Make (Char)
end

let input = "inputs/day6.txt" |> open_in |> input_line |> String.to_seq |> List.of_seq

let rec unique str i n = 
  let sub = List.take n str in
  if sub |> CharSet.of_list |> (fun x -> 
    CharSet.cardinal x = n
    ) then (i + n) else unique (List.tl str) (i + 1) n;;

Printf.printf "part 1 = %d, part 2 = %d\n" (unique input 0 4) (unique input 0 14);;
