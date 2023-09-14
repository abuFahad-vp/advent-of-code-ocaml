open Containers

let input = "inputs/day10.txt" |> open_in |> IO.read_lines_l

let signals =
  let _, signals =
    List.fold_left (fun (sum, acc) line ->
      match String.split_on_char ' ' line with
      | ["noop"] -> (sum, sum :: acc)
      | ["addx";n] -> 
          let sum' = sum + (int_of_string n) in
          (sum', sum' :: sum :: acc)
      | _ -> failwith "Parsing Error"
    ) (1, [1;1]) input
  in
  List.rev signals;;

let part1 =
  List.foldi (fun acc i x -> acc + if (i - 20) mod 40 = 0 then i * x else 0) 0 signals

let part2 =
  let draw i x = if abs (i - x) <= 1 then '#' else ' ' in
  let draw_line = Fun.compose (List.mapi draw) String.of_list in
  signals |> List.drop 1 |> List.chunks 40 |> List.map draw_line |> String.concat "\n"

let _ =
  Printf.printf "part1=%d\npart2=\n%s" part1 part2
