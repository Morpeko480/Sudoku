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

let remove_amount_of_cells level =
  let solved_board = generate_full_board () in
  let all_elements = List.flatten solved_board in
  let indices = List.init 81 (fun i -> i + 1) in
  let coordonated_list = List.combine indices all_elements in
  let removed_list = List.map (fun (ind, x) -> if List.mem ind level then 0 else x) coordonated_list in
  split removed_list

let rec generate difficulty = 
  let board = remove_amount_of_cells difficulty in
  let solution_counter = snd (solve board 0) in
  if solution_counter = 1 then board else generate difficulty

let get_cell (row, col) board =
  List.nth (List.nth board row) col

let compare board1 board2 =
  let flat1 = List.flatten board1 in 
  let flat2 = List.flatten board2 in
  let rec cmp f1 f2 = 
    match f1, f2 with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | x :: xs, y :: ys -> if x <> y then false else cmp xs ys
  in 
    cmp flat1 flat2 
