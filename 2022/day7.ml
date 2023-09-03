let input = "inputs/day7.txt" |> open_in

type system =
  | Directory of string * system list ref
  | ParentDir of system
  | File of int * string

exception Break
  
let create (current:system) name size isdir = 
  let obj  = if isdir then Directory(name,ref [ParentDir(current)]) 
  else File(size, name) in
  match current with
  | Directory(_,sub_r) -> sub_r := !sub_r @ [obj]
  | _ -> ()

let change_dir (current:system ref) name = 
  match !current with
  | Directory(_,sub_r) -> (
      List.iter (fun x -> 
        try
          match x with
          | ParentDir(dir) -> if name = ".." then (current := dir;raise Break)
          | Directory(n,_) -> if n = name then (current := x; raise Break)
          | _ -> ()
        with Break -> ()
        ) !sub_r)
  | _ -> () 

let rec build root current =
  try
    let tokens = input |> input_line |> String.split_on_char ' ' in
    (match tokens with
    | "$"::"cd"::"/"::[] -> current := root
    | "$"::"cd"::name::[] -> change_dir current name
    | "$"::"ls"::[] -> ()
    | "dir"::name::[] -> create !current name 0 true
    | size::name::[] -> create !current name (int_of_string size) false
    | _ -> (););
    build root current
  with End_of_file -> ();;

let rec size_of (root:system) (acc:int list ref) = 
  let dir_size = ref 0 in
  (match root with
    | Directory(_,subdir) ->  (
        List.iter (fun x ->
          match x with
          | File(size,_) -> dir_size := !dir_size + size
          | Directory _-> dir_size := !dir_size + size_of x acc; 
          | _ -> ()) !subdir)
  | _ -> ());
  acc := !dir_size :: !acc;
  !dir_size;;

let values = ref [];;

let root = Directory("/", ref [])
let current = ref root;;

build root current;;
size_of root values;;

let ans = List.fold_left (fun (p1,p2) b -> 
  let x = if b <= 100000 then p1 + b else p1 in
  let required = 30000000 - (70000000 - (List.hd !values)) in
  let y = if b >= required && b < p2 then b else p2 in
  (x,y)
) (0,max_int) !values;;

Printf.printf "part1 = %d, part 2 = %d\n" (fst ans) (snd ans)

(* 
let print_system (dir:system) = 
  let rec print_system_inner (dir:system) space = 
  match dir with
  | Directory (name, subdir) -> 
      Printf.printf "%*s-%s/\n" space "" name;
      ignore(List.map (fun x -> print_system_inner x (space + 2)) !subdir);
  | File(size,name) -> 
      Printf.printf "%*s-%d : %s\n" space "" size name
  (*| ParentDir(x) ->
      match x with
      | Directory(name,_) -> Printf.printf "%*sparent = %s\n" space "" name
      | _ -> ()*)
  | _ -> ()
  in
  print_system_inner dir 0;;
*)
