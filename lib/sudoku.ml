type board = int list list
let contains value row = 
  List.exists (fun x -> x = value) row

let rec take n l acc =
  match l with
  | [] -> []
  | x :: xs -> if acc = n then [] else x :: take n xs (acc + 1)
let rec split l =
  match l with
  | [] -> []
  | _ ->
    let rec take9 l acc =
    match l with
    | [] -> (List.rev acc, [])
    | x :: xs -> if List.length acc = 9 then (List.rev acc, l) else take9 xs (x :: acc) 
    in
    let (piece, xs) = take9 l [] in
    piece :: split xs 
let find_index x xs =
  List.find_index (fun y -> y = x) xs
let missing_numbers numbers =
  List.filter (fun x -> not (List.mem x numbers))  [1;2;3;4;5;6;7;8;9]

let candidates row col box =
  let already_used = row @ col @ box in
  missing_numbers already_used

let rec get_col index board acc = 
  match board with
  | [] -> List.rev acc
  | x :: xs -> get_col index xs ((List.nth x index) :: acc) 

let get_start_box (row, col) =
  ((row / 3) * 3, (col / 3) * 3)

let get_box (start_row, start_col) board =
  List.concat (List.init 3 (fun i -> (List.filteri (fun j _ -> j >= start_col && j < start_col + 3) (List.nth board (start_row + i)))))
let set_cell (row, col) value board =
  List.mapi (fun i current_row -> if i = row then List.mapi (fun j current_value ->  if j = col then value else current_value) current_row else current_row) board

let get_row i board = List.nth board i
let first_empty board = 
  let rec search row board =
  match board with
  | [] -> None
  | x :: xs -> 
    match find_index 0 x with
    | None -> search (row + 1) xs
    | Some value -> Some (row, value)
  in search 0 board

let indices = [
  1; 2; 3; 4; 5; 6; 7; 8; 9;
  10; 11; 12; 13; 14; 15; 16; 17; 18;
  19; 20; 21; 22; 23; 24; 25; 26; 27;
  28; 29; 30; 31; 32; 33; 34; 35; 36;
  37; 38; 39; 40; 41; 42; 43; 44; 45;
  46; 47; 48; 49; 50; 51; 52; 53; 54;
  55; 56; 57; 58; 59; 60; 61; 62; 63;
  64; 65; 66; 67; 68; 69; 70; 71; 72;
  73; 74; 75; 76; 77; 78; 79; 80; 81
]

let shuffle l =
  List.map snd (List.sort compare (List.map (fun x -> (Random.bits (), x)) l))    
let level_easy () = take 30 (shuffle indices) 0
let level_medium () = take 40 (shuffle indices) 0
let level_hard () = take 50 (shuffle indices) 0

let rec solve (board : board) acc = 
  match first_empty board with
  | None -> (Some board, min 2 (acc + 1))
  | Some (row, col) -> 
      let full_row = get_row row board in
      let full_col = get_col col board [] in
      let full_box = get_box (get_start_box (row, col)) board in
      let candidates = candidates full_row full_col full_box in
      let candidates = shuffle candidates in
      try_candidates (row, col) candidates board acc
      
and try_candidates (row, col) candidates board acc =
  if acc >= 2 then (None, acc)
  else
  match candidates with
  | [] -> (None, acc)
  | x :: xs -> 
    let new_board = set_cell (row, col) x board in
    let (solution, new_acc) = solve new_board acc in
    let (other_solution, final_acc) = try_candidates (row, col) xs board new_acc
  in
    match solution with
    | Some solution -> (Some solution, final_acc)
    | None -> (other_solution, final_acc)

let generate_full_board () =
  let empty_board = 
    [[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0];[0;0;0;0;0;0;0;0;0]]
  in  
    match solve empty_board 0 with
    | (None, _) -> []
    | (Some solution, _) -> solution

let remove_amount_of_cells () =
  let solved_board = generate_full_board () in
  let all_elements = List.flatten solved_board in
  let indices = List.init 81 (fun i -> i + 1) in
  let coordonated_list = List.combine indices all_elements in
  let removed_list = List.map (fun (ind, x) -> if List.mem ind (level_hard ()) then 0 else x) coordonated_list in
  split removed_list

let rec generate () = 
  let board = remove_amount_of_cells () in
  let solution_counter = snd (solve board 0) in
  if solution_counter = 1 then board else generate ()

let get_cell (row, col) board =
  List.nth (List.nth board row) col
(*
remove_test:
  [3; 8; 4; 6; 0; 9; 1; 0; 5]; 
  [1; 2; 6; 5; 3; 0; 0; 7; 0];
  [7; 9; 0; 4; 1; 0; 6; 0; 3]; 
  [0; 4; 9; 0; 8; 1; 2; 5; 7];
  [5; 3; 8; 7; 0; 6; 0; 0; 0]; 
  [2; 0; 7; 9; 4; 5; 3; 0; 8];
  [9; 6; 0; 1; 5; 7; 8; 3; 0]; 
  [4; 0; 1; 8; 0; 3; 7; 0; 2];
  [8; 7; 3; 0; 9; 0; 5; 1; 6]]






let board1 = [
  [0; 0; 3; 0; 2; 0; 6; 0; 0];
  [9; 0; 0; 3; 0; 5; 0; 0; 1];
  [0; 0; 1; 8; 0; 6; 4; 0; 0];
  [0; 0; 8; 1; 0; 2; 9; 0; 0];
  [7; 0; 0; 0; 0; 0; 0; 0; 8];
  [0; 0; 6; 7; 0; 8; 2; 0; 0];
  [0; 0; 2; 6; 0; 9; 5; 0; 0];
  [8; 0; 0; 2; 0; 3; 0; 0; 9];
  [0; 0; 5; 0; 1; 0; 3; 0; 0]
]

Solution:
  [4; 8; 3; 9; 2; 1; 6; 5; 7]; 
  [9; 6; 7; 3; 4; 5; 8; 2; 1];
  [2; 5; 1; 8; 7; 6; 4; 9; 3]; 
  [5; 4; 8; 1; 3; 2; 9; 7; 6];
  [7; 2; 9; 5; 6; 4; 1; 3; 8]; 
  [1; 3; 6; 7; 9; 8; 2; 4; 5];
  [3; 7; 2; 6; 8; 9; 5; 1; 4]; 
  [8; 1; 4; 2; 5; 3; 7; 6; 9];
  [6; 9; 5; 4; 1; 7; 3; 8; 2]





Easy  :
  [5; 3; 0; 0; 7; 0; 0; 0; 0];
  [6; 0; 0; 1; 9; 5; 0; 0; 0];
  [0; 9; 8; 0; 0; 0; 0; 6; 0];
  [8; 0; 0; 0; 6; 0; 0; 0; 3];
  [4; 0; 0; 8; 0; 3; 0; 0; 1];
  [7; 0; 0; 0; 2; 0; 0; 0; 6];
  [0; 6; 0; 0; 0; 0; 2; 8; 0];
  [0; 0; 0; 4; 1; 9; 0; 0; 5];
  [0; 0; 0; 0; 8; 0; 0; 7; 9]]

Solved  :

[ [5; 3; 4; 6; 7; 8; 9; 1; 2]; 
  [6; 7; 2; 1; 9; 5; 3; 4; 8];
  [1; 9; 8; 3; 4; 2; 5; 6; 7]; 
  [8; 5; 9; 7; 6; 1; 4; 2; 3];
  [4; 2; 6; 8; 5; 3; 7; 9; 1]; 
  [7; 1; 3; 9; 2; 4; 8; 5; 6];
  [9; 6; 1; 5; 3; 7; 2; 8; 4]; 
  [2; 8; 7; 4; 1; 9; 6; 3; 5];
  [3; 4; 5; 2; 8; 6; 1; 7; 9]]
*)