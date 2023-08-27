type space = OCCUPIED | EMPTY | NONBOARD;;

(*
The board is conceptually a 2D space, but is implemented as a hash map to each square's index (49 items)
xxOOOxx
xxOOOxx
OOOOOOO
OOOEOOO
OOOOOOO
xxOOOxx
xxOOOxx
*)
let non_board_spaces = [0; 1; 5; 6; 7; 8; 12; 13; 35; 36; 40; 41; 42; 43; 47; 48];;

let minimum_stones = ref 32;;

let rec make_initial_board current_board fill_square =
  match fill_square with
  | -1 -> current_board
  | 24 -> let () = Hashtbl.add current_board 24 EMPTY in
    make_initial_board current_board (fill_square - 1)
  | _ -> if List.mem fill_square non_board_spaces then
      let () = Hashtbl.add current_board fill_square NONBOARD in
      make_initial_board current_board (fill_square - 1)
    else let () = Hashtbl.add current_board fill_square OCCUPIED in make_initial_board current_board (fill_square - 1);;

let initial_board = make_initial_board (Hashtbl.create 49) 48;;

let index_to_x_y n = ((n mod 7), (n / 7));;
let x_y_to_index (x, y) = (y * 7) + x;;
let get_repr item = match item with
 | OCCUPIED -> 'O'
 | EMPTY -> '_'
 | NONBOARD -> 'x';;

let print_board_list board = List.iteri (fun index space -> let (x, y) = index_to_x_y index in
  if (x mod 7 == 0) then print_newline();
  print_char (get_repr space)
) board;;

let rec make_board_list board n board_list =
  match n with
  | 49 -> board_list
  | _ -> if Hashtbl.mem board n then
    make_board_list board (n + 1) (board_list @ [Hashtbl.find board n])
  else make_board_list board (n + 1) (board_list @ [NONBOARD])
;;

let make_board_from_hashtbl hash_board = make_board_list hash_board 0 [];;

let print_board hash_board = print_board_list (make_board_from_hashtbl hash_board); print_newline()
;;

let get_between a b = if a > b then a - 1 else a + 1;;
let get_between_index index_a index_b = let (x1, y1) = index_to_x_y index_a in
    let (x2, y2) = index_to_x_y index_b in
    if x1 == x2 then x_y_to_index (x1, get_between y1 y2) else x_y_to_index (get_between x1 x2, y1)
  ;;

let make_horizontal_move board y xfrom xto = 
  Hashtbl.replace board (x_y_to_index (xfrom, y)) EMPTY;
  Hashtbl.replace board (x_y_to_index (get_between xfrom xto, y)) EMPTY;
  Hashtbl.replace board (x_y_to_index (xto, y)) OCCUPIED;
  board
;;

let make_vertical_move board x yfrom yto =
  Hashtbl.replace board (x_y_to_index (x, yfrom)) EMPTY;
  Hashtbl.replace board (x_y_to_index (x, get_between yfrom yto)) EMPTY;
  Hashtbl.replace board (x_y_to_index (x, yto)) OCCUPIED;
  board
;;

let make_move board index_from index_to = let (x1, y1) = index_to_x_y index_from in
  let (x2, y2) = index_to_x_y index_to in
  if x1 == x2 then make_vertical_move board x1 y1 y2
  else make_horizontal_move board y1 x1 x2
;;

let in_range board_index = board_index > -1 && board_index < 49 && not (List.mem board_index non_board_spaces)

let valid_x_y (x, y) = x >= 0 && y >= 0 && x < 7 && y < 7;;

let can_make_move board index_from index_to = 
  in_range index_from
  && in_range index_to
  && Hashtbl.find board index_from == OCCUPIED
  && Hashtbl.find board index_to == EMPTY
  && Hashtbl.find board (get_between_index index_from index_to) == OCCUPIED
;;

let stones_left board = Hashtbl.to_seq board |> Seq.filter (fun (_, elem) -> elem == OCCUPIED) |> Seq.length;;

let is_win board = stones_left board == 1;;

let print_bool input = if input then print_string "true" else print_string "false";;

let get_possible_moves_from_index board index =
  let (x, y) = index_to_x_y index in
  let moves_to_check = [(x + 2, y); (x - 2, y); (x, y + 2); (x, y - 2)] |> List.filter (valid_x_y) in
  List.map (fun item -> x_y_to_index item) moves_to_check
  |> List.filter (fun to_index -> can_make_move board index to_index)
  |> List.map (fun move_index -> (index, move_index))
;;

let get_all_possible_moves board = 
  Hashtbl.to_seq board
  |> Seq.map (fun (ind, item) ->
    match item with 
    | OCCUPIED -> get_possible_moves_from_index board ind
    | _ -> []
  )
  |> List.of_seq |> List.flatten;;

let update_min_stones board min_ref = 
  let left = stones_left board in
  if left < !min_ref then (min_ref := left; print_int left; print_newline ()) else ()
;;

let rec solve_with_moves board possible_moves = 
update_min_stones board minimum_stones;
if List.length possible_moves == 0 then is_win board else
  let (try_index_to, try_index_from) = List.nth possible_moves 0 in
  if solve (make_move (Hashtbl.copy board) try_index_to try_index_from) then let () = print_board board in true else
    solve_with_moves board (List.tl possible_moves)
and
solve board = let possible_moves = get_all_possible_moves board in
solve_with_moves board possible_moves
;;

solve initial_board;;
