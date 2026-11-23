
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

  let board : Sudoku.board ref = ref (Sudoku.generate ()) in

  let window =
    GWindow.window
      ~title:"Sudoku"
      ~width:520
      ~height:620
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
  let generate_button = 
    GButton.button
      ~label:"New Sudoku"
      ~packing:main_box#add
      ()
  in
  let solve_button =
    GButton.button
    ~label:"Solve"
    ~packing:main_box#add
    ()
  in
  let print_buttons () =
    List.iter grid#remove grid#children;
    for row = 0 to 8 do
      for col = 0 to 8 do

        let value = Sudoku.get_cell (row, col) !board in
        let value_string = if value = 0 then " " else string_of_int value in
        let button = GButton.button ~label:value_string () in

        let style = button#misc#style_context in

        style#add_class "sudoku-cell";
        if col = 2 || col = 5 then style#add_class "box-right";
        if row = 2 || row = 5 then style#add_class "box-bottom";

        grid#attach button#coerce
          ~left:col
          ~top:row
          ~width:1
          ~height:1;
      done
    done
  in ignore(generate_button#connect#clicked 
    ~callback: 
    (fun () -> 
      board := Sudoku.generate (); 
      print_buttons ()
    ));
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

  let _ = window#connect#destroy ~callback:GMain.quit in

  window#show ();
  GMain.main ()
