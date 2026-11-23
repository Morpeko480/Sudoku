type board = int list list

val contains : 'a -> 'a list -> bool
val take : int -> 'a list -> int -> 'a list
val split : 'a list -> 'a list list
val find_index : 'a -> 'a list -> int option
val missing_numbers : int list -> int list
val candidates : int list -> int list -> int list -> int list
val get_col : int -> board -> int list -> int list
val get_start_box : int * int -> int * int
val get_box : int * int -> board -> int list
val set_cell : int * int -> int -> board -> board
val get_row : int -> board -> int list
val first_empty : board -> (int * int) option
val indices : int list
val shuffle : 'a list -> 'a list
val level_easy : unit -> int list
val level_medium : unit -> int list
val level_hard : unit -> int list
val solve : board -> int -> board option * int
val try_candidates : int * int -> int list -> board -> int -> board option * int
val generate_full_board : unit -> board
val remove_amount_of_cells : unit -> board
val generate : unit -> board
val get_cell : int * int -> board -> int
