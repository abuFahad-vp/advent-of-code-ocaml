open Containers
let inputs = "inputs/day11.txt" |> open_in |> IO.read_lines_l |> List.chunks 7 
              |> List.map Array.of_list;;

type monkey = {
  count : int ref;
  items : int list ref;
  inspected : int ref;
  operation : int -> int;
  throw : int -> int;
}

let parse_block blk arr index = 
  let items = blk.(1) |> Str.split (Str.regexp "[ a-zA-Z:,]+") |> List.map int_of_string in
  let op = blk.(2) |> Str.split (Str.regexp "[ A-Z:,]+") |> List.drop 4 in
  let op = 
    match op with
    | ["+";"old"] -> (fun x -> x + x)
    | ["*";"old"] -> (fun x -> x * x)
    | ["+";v] -> (fun x -> x + (int_of_string v))
    | ["*";v] -> (fun x -> x * (int_of_string v))
    | _ -> failwith "invalid operation"
  in
  let throw_value = blk.(3) |> Str.split (Str.regexp "[ a-zA-Z:,]+") |> List.hd |> int_of_string in
  let mon1 = blk.(4) |> Str.split (Str.regexp "[ a-zA-Z:,]+") |> List.hd |> int_of_string in
  let mon2 = blk.(5) |> Str.split (Str.regexp "[ a-zA-Z:,]+") |> List.hd |> int_of_string in

  let monkey = {count = ref (List.length items);
  items = ref items;
  operation = op;
  inspected = ref 0;
  throw = (fun x -> if x mod throw_value = 0 then mon1 else mon2);
  } in
  arr.(index) <- monkey;
  (monkey,throw_value);;


let do_round inputs n div =

  let dummy = {count = ref 1;items = ref [];inspected = ref 0;operation = (fun x -> x);throw = (fun x -> x);} in
  let monkeys = Array.make (List.length inputs) dummy  in
  let throw_pro = ref 1 in

  List.iteri (fun i v -> let _,t = parse_block v monkeys i in throw_pro := !throw_pro * t) inputs;

  let rec do_round' monkeys n div = 
    if n > 0 then
    begin
    Array.iter (fun monk ->
      monk.inspected := !(monk.inspected) + !(monk.count);
      List.iter (fun x -> 
        let throw_val = x |> monk.operation |> (fun x -> (x / div) mod !throw_pro) in
        let throw_to = throw_val |> monk.throw in
        let catch_monk = monkeys.(throw_to) in

        catch_monk.items := throw_val :: !(catch_monk.items);

        incr catch_monk.count 

      ) !(monk.items);
      monk.items := [];
      monk.count := 0
    ) monkeys;
    do_round' monkeys (n - 1) div
    end
    else
      (let insp = Array.fold_left (fun acc monk -> !(monk.inspected) :: acc) [] monkeys in
      match List.sort (fun a b -> b - a) insp with
      | a::b::_ -> a * b
      | _ -> failwith "I don't know"); 
    in
    do_round' monkeys n div
;;    
Printf.printf "part 1 = %d, part 2 = %d\n" (do_round inputs 20 3) (do_round inputs 10000 1);;

(*
let block = 
  "Monkey 1:
    Starting items: 54, 65, 75, 74, 78
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0" |> String.split_on_char '\n' |> Array.of_list;;

let arr = Array.make 1 dummy;;
let _ = parse_block block arr 0;;
let print_monkey mon x = 
  Printf.printf "Count = %d\n" !(mon.count);
  Printf.printf "Items = "; 
  List.iter (Printf.printf "%d ") !(mon.items);
  Printf.printf "\nInspected so far = %d\n" !(mon.inspected);
  Printf.printf "Operation with 1 = %d\n" (mon.operation 1); 
  Printf.printf "Throw with %d = %d\n" x (mon.throw x);;

print_monkey dummy 10;;
*)

