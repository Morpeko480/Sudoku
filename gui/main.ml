
let () =
  let _ = GMain.init () in

  let css_provider = GtkData.CssProvider.create () in
  let channel = open_in "gui/style.css" in
  let css_data =
    really_input_string channel (in_channel_length channel)
  in
  close_in channel;

  GtkData.CssProvider.load_from_data css_provider css_data;

  let screen = Gdk.Screen.default () in

  GtkData.StyleContext.add_provider_for_screen
  screen
  css_provider
  GtkData.StyleContext.ProviderPriority.application;

  let level : int list ref = ref (Sudoku.level_easy ()) in
  let board : Sudoku.board ref = ref (Sudoku.generate !level) in
  let solved_board : Sudoku.board ref = match Sudoku.solve !board 0 with | (None, _) -> ref [[]] | (Some board, _) -> ref board in  
  let playing_board : Sudoku.board ref = ref !board in
  let checked = ref false in

  let add_style button style_name =
  button#misc#style_context#add_class style_name
  in
  let window =
    GWindow.window
      ~title:"Sudoku"
      ~width:400
      ~height:600
      ~border_width:12
      ~decorated:true
      ()
  in
  let main_box = 
    GPack.vbox
      ~spacing:10
      ~packing:window#add
      ()
  in
  let grid =
    GPack.grid
      ~row_spacings:0
      ~col_spacings:0
      ~packing:main_box#add
      ()
  in
  let solve_button =
    GButton.button
    ~label:"Solve"
    ~packing:main_box#add
    ()
  in
  add_style solve_button "solve_button";
  solve_button#misc#hide ();
  let easy_button =
    GButton.button
    ~label:"Easy"
    ~packing:main_box#add
    ()
  in
  add_style easy_button "easy_button";
  let medium_button =
    GButton.button
    ~label:"Medium"
    ~packing:main_box#add
    ()
  in
  add_style medium_button "medium_button";
  let hard_button =
    GButton.button
    ~label:"Hard"
    ~packing:main_box#add
    ()
  in
  add_style hard_button "hard_button";
  let print_buttons () =
    List.iter grid#remove grid#children;
    for row = 0 to 8 do
      for col = 0 to 8 do

        let original_value = Sudoku.get_cell (row, col) !board in
        let value = Sudoku.get_cell (row, col) !playing_board in
        let value_string = if value = 0 then "" else string_of_int value in
        let entry =
          GEdit.entry
            ~text:""
            ~width_chars:2
            ~max_length:1
            ()
        in
        entry#set_text value_string;
        entry#set_xalign 0.5;
        entry#set_editable (original_value = 0);
        let style = entry#misc#style_context in

        let update_feedback () =
          let current = Sudoku.get_cell (row, col) !playing_board in
          let solution = Sudoku.get_cell (row, col) !solved_board in

          style#remove_class "correct";
          style#remove_class "mistake";

          if !checked && original_value = 0 && current <> 0 then
            if current = solution then
              style#add_class "correct"
            else
              style#add_class "mistake"
        in
        ignore(entry#connect#changed 
        ~callback:(fun () -> 
          let text = entry#text in
          match int_of_string_opt text with
          | Some n when n >= 1 && n <= 9 -> playing_board := Sudoku.set_cell (row, col) n !playing_board; update_feedback ()
          | _ when text = "" -> playing_board := Sudoku.set_cell (row, col) 0 !playing_board; update_feedback ()
          | _ -> entry#set_text ""
        ));
        
        style#add_class "sudoku-cell";
        if col = 2 || col = 5 then style#add_class "box-right";
        if row = 2 || row = 5 then style#add_class "box-bottom";
        update_feedback ();
        grid#attach entry#coerce
          ~left:col
          ~top:row
          ~width:1
          ~height:1;
      done
    done
  in 
  let new_game difficulty =
    level := difficulty ();
    board := Sudoku.generate !level;
    playing_board := !board;
    checked := false;
    solved_board :=(
      match (Sudoku.solve !board 0) with
      | (Some solution, _) -> solution
      | (None, _) -> failwith "Generated Sudoku with no solution!");
      solve_button#misc#show ();
      print_buttons ()
  in 
    ignore (solve_button#connect#clicked
    ~callback:
    (fun () ->
      checked := true;
      print_buttons ()
    ));
    ignore (easy_button#connect#clicked
    ~callback:
    (fun () -> new_game Sudoku.level_easy));
    ignore (medium_button#connect#clicked
    ~callback:
     (fun () -> new_game Sudoku.level_medium));
    ignore (hard_button#connect#clicked
    ~callback:
     (fun () -> new_game Sudoku.level_hard));

  let _ = window#connect#destroy ~callback:GMain.quit in

  window#show ();
  GMain.main ()
