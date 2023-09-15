open Containers

let stacks () = "inputs/day5.txt" |> open_in |> IO.read_lines_l |> List.map (
  fun x -> x |> String.rev |> String.to_seq |> Stack.of_seq) |> Array.of_list;;

let stacks1 = stacks ();;
let stacks2 = stacks ();;

let commands = "inputs/day5_.txt" |> open_in |> IO.read_lines_l |> List.map (
  fun x -> 
    let lst = x |> String.split_on_char ' ' |> Array.of_list in
    [|int_of_string lst.(1);int_of_string lst.(3);int_of_string lst.(5)|])

let rec move1 n st1 st2 = if n > 0 then (Stack.push (Stack.pop st1) st2; move1 (n - 1) st1 st2);;

let move2 n st1 st2 = 
  let temp = Stack.create () in
  move1 n st1 temp;
  move1 n temp st2;;

List.iter (fun cmnd ->
  move1 cmnd.(0) (stacks1.(cmnd.(1) - 1)) (stacks1.(cmnd.(2) - 1));
  move2 cmnd.(0) (stacks2.(cmnd.(1) - 1)) (stacks2.(cmnd.(2) - 1))
) commands;;

let print_stack_top stack part = 
  Printf.printf "part %d = " part;
  Array.iter (fun x -> Printf.printf "%c" (Stack.pop x)) stack; print_endline "";;

print_stack_top stacks1 1;;
print_stack_top stacks2 2;;
