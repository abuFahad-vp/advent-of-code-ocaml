let file = "inputs/day3.txt"

module CharSet = struct
  include Set.Make (Char)
  let of_string s = s |> String.to_seq |> of_seq
end

let common array_string = 
  let open CharSet in
  let head = of_string (List.hd array_string) in
  let prio = array_string |> List.map of_string |> List.fold_left inter head |> choose |> int_of_char in
  if prio > 96 then prio - 96 else prio - 38

let _ = 
  let rec priority_sum ic ans1 ans2 = 
    try
      let line1 = input_line ic in
      let line2 = input_line ic in
      let line3 = input_line ic in
      let value1 = List.fold_left (fun a b -> 
        let length = String.length b in
        a + common [
          String.sub b 0 (length/ 2);
          String.sub b (length / 2) (length / 2);
      ]) 0 [line1;line2;line3]
      in
      let value2 = common [line1;line2;line3] in
      priority_sum ic (ans1 + value1) (ans2 + value2);
    with _ -> Printf.printf "part1 = %d part2 = %d\n" ans1 ans2;0
  in 
  priority_sum (open_in file) 0 0
