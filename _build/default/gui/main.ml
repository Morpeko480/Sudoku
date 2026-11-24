
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

        let value = Sudoku.get_cell (row, col) !playing_board in
        let value_string = if value = 0 then "" else string_of_int value in
        let entry =
          GEdit.entry
            ~text:""
            ~width_chars:2
            ~max_length:1
            ()
        in
        let compare () = let style = entry#misc#style_context in if Sudoku.compare !playing_board !solved_board then style#add_class "correct" else style#add_class "mistake" in
        entry#set_text value_string;
        entry#set_xalign 0.5;
        entry#set_editable (value = 0);
        ignore(entry#connect#changed 
        ~callback:(fun () -> 
          let text = entry#text in
          match int_of_string_opt text with
          | Some n when n >= 1 && n <= 9 -> playing_board := Sudoku.set_cell (row, col) n !playing_board
          | _ when text = "" -> playing_board := Sudoku.set_cell (row, col) 0 !playing_board
          | _ -> entry#set_text ""
        ));
        compare ();
        let style = entry#misc#style_context in

        style#add_class "sudoku-cell";
        if col = 2 || col = 5 then style#add_class "box-right";
        if row = 2 || row = 5 then style#add_class "box-bottom";

        grid#attach entry#coerce
          ~left:col
          ~top:row
          ~width:1
          ~height:1;
      done
    done
  in 
    ignore (solve_button#connect#clicked
    ~callback:
    (fun () ->
      let board_option = fst (Sudoku.solve !board 0) in
      (
      match board_option with
      | None -> ()
      | Some solution -> board := solution
      );
      print_buttons ()
    ));
    ignore (easy_button#connect#clicked
    ~callback:
    (fun () ->
      level := Sudoku.level_easy ();
      board := Sudoku.generate !level;
      solve_button#misc#show ();
      print_buttons ()
    ));
    ignore (medium_button#connect#clicked
    ~callback:
    (fun () ->
      level := Sudoku.level_medium ();
      board := Sudoku.generate !level;
      solve_button#misc#show ();
      print_buttons ()
    ));
    ignore (hard_button#connect#clicked
    ~callback:
    (fun () ->
      level := Sudoku.level_hard ();
      board := Sudoku.generate !level;
      solve_button#misc#show ();
      print_buttons ()
    ));

  let _ = window#connect#destroy ~callback:GMain.quit in

  window#show ();
  GMain.main ()
