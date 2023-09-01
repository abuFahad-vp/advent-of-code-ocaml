open Containers

let stacks = "inputs/day5.txt" |> open_in |> IO.read_lines_l |> List.map (
  fun x -> x |> String.rev |> String.to_seq |> Stack.of_seq) |> Array.of_list;;

let commands = "inputs/day5_.txt" |> open_in |> IO.read_lines_l |> List.map (
  fun x -> 
    let lst = x |> String.split_on_char ' ' |> Array.of_list in
    [|int_of_string lst.(1);int_of_string lst.(3);int_of_string lst.(5)|])

let rec move n st1 st2 = if n > 0 then (Stack.push (Stack.pop st1) st2; move (n - 1) st1 st2);;

List.iter (fun cmnd ->
  move cmnd.(0) (stacks.(cmnd.(1) - 1)) (stacks.(cmnd.(2) - 1))
) commands;;

Array.iter (fun x -> Printf.printf "%c" (Stack.pop x)) stacks;;
print_endline "";;
