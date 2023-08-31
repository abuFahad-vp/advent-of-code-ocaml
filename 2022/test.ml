let file = "day1.txt"

let sol1 () = 
  let rec sum ic acc values = 
    try
      let line = (input_line ic) in
      sum ic (acc + (int_of_string line)) values
    with  
      | Failure (_) -> 
          sum ic 0 (acc :: values)
      
      | End_of_file -> List.sort (fun a b -> b - a) (acc::values) in

    match sum (open_in file) 0 [] with
    | a::b::c::_ -> (a + b + c)
    | _ -> 0
;;

let sol2 () = 
  let rec sum ic acc a b c = 
    try
      let line = (input_line ic) in
      sum ic (acc + (int_of_string line)) a b c
    with  
      | Failure (_) ->
          sum ic 0 (max a acc) (max b (min a acc)) (max c (min b acc))
      
      | End_of_file -> 
          let a1, a2, a3 = 
              (max a acc), (max b (min a acc)),(max c (min b acc))
          in
          (a1 + a2 + a3)
  in 
  sum (open_in file) 0 0 0 0;;

(* Benchmarking the two functions *)

let measure_execution_time f =
  let start_time = Unix.gettimeofday () in
  let result = f () in
  let end_time = Unix.gettimeofday () in
  result, end_time -. start_time

let () =
  let result_a, time_a = measure_execution_time sol1 in
  let result_b, time_b = measure_execution_time sol2 in

  Printf.printf "Function A result: %s\n" (string_of_int result_a);
  Printf.printf "Function A execution time: %.6f seconds\n" time_a;

  Printf.printf "Function B result: %s\n" (string_of_int result_b);
  Printf.printf "Function B execution time: %.6f seconds\n" time_b;
