open Containers
let input = "inputs/day4.txt" |> open_in |> IO.read_lines_l

let count = ref 0
let count_no_lap = ref 0;;

List.iter (fun x ->
  let nums = x |> Str.split (Str.regexp "[,-]") |> List.map int_of_string in
  match nums with
  | a::b::c::d::_ -> 
      if a <= c && b >= d then incr count else 
      if c <= a && d >= b then incr count else ();

      if a <= c && b >= c then incr count_no_lap else 
      if c <= a && d >= a then incr count_no_lap else ()
  | _ -> ()
) input;;

Printf.printf "part 1 = %d, part 2 = %d\n" !count !count_no_lap
