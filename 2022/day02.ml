let file = "inputs/day2.txt"

let score op pl = 
  match op - pl with
  | 4 -> pl + 3
  | 5 | 2 -> pl
  | 6 | 3 -> pl + 6
  | _ -> 0;;

let rec game ic ans1 ans2 = 
  try
    let pl, op = int_of_char (input_char ic) , int_of_char (input_char ic) in
    let pl1 = pl - 87 in
    let pl2 = (op + pl - 148) mod 3 + 1 in
    let op = op - 60 in
    ignore(input_char ic);
    game ic (ans1 + (score op pl1)) (ans2 + (score op pl2))
  with 
  | End_of_file -> Printf.printf "part1 =  %d, part2 = %d\n" ans1 ans2; close_in ic;
  | e -> close_in ic; raise e;;

game (open_in file) 0 0
