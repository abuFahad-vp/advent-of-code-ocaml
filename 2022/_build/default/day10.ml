open Containers

let signals =
  let _, signals =
    List.fold_left (fun (sum, acc) line ->
      match String.split_on_char ' ' line with
      | ["noop"] -> (sum, sum :: acc)
      | ["addx";n] -> 
          let sum' = sum + (int_of_string n) in
          (sum', sum' :: sum :: acc)
      | _ -> failwith "Parsing Error"
    ) (1, [1]) ("inputs/day10.txt" |> open_in |> IO.read_lines_l)
  in
  List.rev signals;;

let () =
  let ans = List.foldi (fun acc i x -> 
    if i mod 40 = 0 then Printf.printf "\n";
    if abs((i mod 40) - x) <= 1 then print_string "##" else print_string "  ";
    acc + if (i - 20) mod 40 = 0 then i * x else 0) 0 signals in
    Printf.printf "\npart1 = %d\n" ans

