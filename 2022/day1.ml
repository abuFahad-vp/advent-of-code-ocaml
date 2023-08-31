let file = "inputs/day1.txt"

let rec sum ic acc values = 
  try
    let line = (input_line ic) in
    sum ic (acc + (int_of_string line)) values
  with  
    | Failure (_) -> 
        sum ic 0 (acc :: values)
    
    | End_of_file -> List.sort (fun a b -> b - a) (acc :: values);;

match sum (open_in file) 0 [] with
|a::b::c::_ -> Printf.printf "part 1 = %d, part 2 = %d\n" a (a + b + c);
| _ -> ()
