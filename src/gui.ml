open Bogue
open Main
open Pacmap
open Movable

(* open Camel open Human *)
open Utils
open State
open Random
module W = Widget
module L = Layout
module T = Trigger

(* SETUP *)
(* let width = 800 let height = 800 *)

(* WIDGETS*)
(* let start_title_w = W.label ~size:32 ~fg:Draw.(opaque (find_color
   "firebrick")) "The Pac-Camel Game" *)

(* let start_button_w = W.button "Press [s] to Start the Game" *)
(*camel lives *)
let start = W.image "assets/images/start.png"
let start_l = L.resident start
let life1 = W.image "assets/images/camel.png" ~w:15 ~h:15
let life2 = W.image "assets/images/camel.png" ~w:15 ~h:15 ~angle:10.0
let life3 = W.image "assets/images/camel.png" ~w:15 ~h:15 ~angle:20.0
let score = state_score

let score_w =
  W.label
    ("Score: " ^ string_of_int !score)
    ~fg:Draw.(opaque (find_color "black"))
    ~size:10

let score_l = L.resident score_w ~x:825 ~w:100 ~h:100 ~y:240

let pause_instruction =
  W.label "Press [p] to Pause/Resume "
    ~fg:Draw.(opaque (find_color "black"))
    ~size:7

let pause_instruction_l =
  L.resident pause_instruction ~x:820 ~w:170 ~h:100 ~y:700

let restart_instruction =
  W.label "Press [r] to get a new map "
    ~fg:Draw.(opaque (find_color "black"))
    ~size:7

let restart_instruction_l =
  L.resident restart_instruction ~x:820 ~w:170 ~h:100 ~y:680

let display_time =
  W.label
    ("Timer: " ^ string_of_int ((!state_end_time - !state_time) / 100))
    ~fg:Draw.(opaque (find_color "black"))
    ~size:10

let display_time_l = L.resident display_time ~x:832 ~w:100 ~h:100 ~y:300

let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque black) ~width:3 ~style:Solid ()

let round_blue_box =
  let open Style in
  let border = mk_border ~radius:2 thick_grey_line in
  create ~border ~background:(color_bg Draw.(opaque @@ find_color "black")) ()

(* let bg_off = Style.color_bg Draw.none let bg_over = Some (Style.opaque_bg
   Draw.grey) *)
(* let fg = Draw.(opaque black) *)
(*LAYOUT*)

(* TODO @GUI (optional): animate in the title + add hovering to buttons upon
   user mouse hover event *)
(* TODO (add keyboard options for quit (Q), play/pause (spacebar or P), and H (H
   or ?); these would trigger events that the buttons that they correspond to
   trigger *)
(* let start_title_l = L.resident start_title_w ~y:2 let start_button_l =
   L.resident ~x:30 ~y:35 ~w:250 ~h:2 start_button_w *)

(* TODO @GUI (post-MS2): Implement more widgets 
 *  - score displayer
 *  - time displayer (perhaps we can set a time limit for the player)
 *  - a play/pause button
 *  - a reset button (has the same action as start button, just with the text changed) 
 *  - a text field to generate a map with a specific seed -> a button to apply (or just reset) 
 *  - a help button to open up a popup that shows the user the different features that are implemented
 *  - (optional) an algorithm selection drop down menu that selects what algorithm that the humans follow 
 *  - (optional) a speed and number of humans setting (set a minimum and maximum)
 *)

let canvas = W.sdl_area ~w:800 ~h:800 ~style:round_blue_box ()
let canvas_l = L.resident ~x:0 ~y:0 canvas
let sdl_area = W.get_sdl_area canvas

(* reference to map *)
let map_ref = ref (gen_map (int 500) sdl_area)

(* reference to camel *)
let camel_ref = ref (Camel.init !map_ref "assets/images/camel.png")

(* references to humans *)

let rec human_inits acc n =
  if n = 0 then acc
  else
    human_inits
      (ref (Human.init !map_ref "assets/images/human.png") :: acc)
      (n - 1)

let human_ref_lst = ref (List.rev (human_inits [] 4))

(*Initialize game state*)
let state = init_state ()

let reset_map (seed : int) =
  (* reset canvas *)
  Sdl_area.clear sdl_area;
  map_ref := gen_map seed sdl_area;
  camel_ref := Camel.init !map_ref "assets/images/camel.png";
  human_ref_lst := human_inits [] 4;
  draw_map sdl_area !map_ref

let time_ref = ref 0
let reset_time () = time_ref := Time.now ()

(* reset states *)
let reset_states () =
  state_time := 0;
  state_score := 0;
  state_lives := 3;
  reset_state_camel ();
  reset_state_human ()

(* sets up the game *)
let reset_game seed =
  reset_time ();
  reset_states ();
  reset_map seed

let bg = (255, 255, 255, 255)

let make_greeting_board =
  let greeting_layout = L.superpose [ start_l ] in
  L.set_width greeting_layout 800;
  L.set_height greeting_layout 800;

  of_layout greeting_layout

let lives_l = L.flat_of_w [ life1; life2; life3 ]

let make_game_board =
  L.set_height lives_l 60;
  L.set_width lives_l 150;
  L.set_show lives_l false;
  L.setx lives_l 850;
  L.sety lives_l 150;
  (* set what to be drawn *)
  (* TODO @GUI: clicking on widgets do not work: try to fix *)
  let layout =
    L.superpose
      [
        canvas_l;
        score_l;
        lives_l;
        display_time_l;
        pause_instruction_l;
        restart_instruction_l;
        Space.hfill ();
      ]
  in
  L.set_width layout 481;
  L.set_height layout 385;
  L.fix_content layout;
  of_layout layout

let final_score =
  W.label
    ("Your Final Score is : " ^ string_of_int !score)
    ~fg:Draw.(opaque (find_color "white"))
    ~align:Center ~size:20

let make_win_board =
  let final_score_l = L.resident final_score ~x:0 ~y:0 in
  let instruction =
    W.label "Press [r] to restart"
      ~fg:Draw.(opaque (find_color "white"))
      ~align:Center
  in
  let instruction_l = L.resident instruction ~x:35 ~y:35 in
  let p = Image.create ~bg:Draw.(opaque white) "assets/images/win.png" in
  let layout =
    L.superpose
      [ final_score_l; instruction_l ]
      ~background:(L.style_bg (Style.create ~background:(Style.image_bg p) ()))
  in
  L.set_width layout 500;
  L.set_height layout 400;
  of_layout layout

let make_lose_board =
  let final_score_l = L.resident final_score ~x:0 ~y:0 in
  let instruction =
    W.label "Press [r] to restart"
      ~fg:Draw.(opaque (find_color "white"))
      ~align:Center
  in
  let p = Image.create ~bg:Draw.(opaque white) "assets/images/lose1.png" in
  let instruction_l = L.resident instruction ~x:35 ~y:35 in
  let layout =
    L.superpose
      [ final_score_l; instruction_l ]
      (* ~background: *)
      (* (L.style_bg (Style.create ~background:(Style.opaque_bg Draw.black)
         ())) *)
      ~background:(L.style_bg (Style.create ~background:(Style.image_bg p) ()))
  in

  L.set_width layout 500;
  L.set_height layout 400;
  of_layout layout

let board = ref make_greeting_board

(*Helper functions to change board*)
let change_game_board () =
  reset_states ();
  board := make_game_board;
  change_state state Active

(*The main gameloop*)
let main () =
  let open Tsdl in
  Sys.catch_break true;
  go (Sdl.init Sdl.Init.video);
  let win =
    go
      (Sdl.create_window ~w:1000 ~h:800 "Pac-Camel Game"
         Sdl.Window.(shown + popup_menu))
  in

  let renderer = go (Sdl.create_renderer win) in
  let load_texture src =
    let surface = Tsdl_image.Image.load src in
    let t = Sdl.create_texture_from_surface renderer (go surface) in
    go t
  in

  (* TODO @Yaqi: camel texture has a black background while human does not; try
     to fix that bug *)
  let camel_texture = load_texture (Camel.src !camel_ref) in
  let coin_texture = load_texture "assets/images/items/coin.png" in
  let coin_pile_texture = load_texture "assets/images/items/coin_pile.png" in
  let phase_texture = load_texture "assets/images/items/potion.png" in
  let sand_texture = load_texture "assets/images/items/sand.jpg" in
  let human_texture = load_texture "assets/images/human.png" in
  let heart_texture = load_texture "assets/images/items/heart.png" in
  let speed_texture = load_texture "assets/images/items/speed.png" in
  let time_texture = load_texture "assets/images/items/time.png" in
  let cactus_texture = load_texture "assets/images/items/cactus.png" in
  let shield_texture = load_texture "assets/images/items/shield.png" in

  let camel_facing = ref false in
  (* very important: set blend mode: *)
  go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);

  go (Sdl.render_clear renderer);

  make_sdl_windows ~windows:[ win ] !board;
  let start_fps, fps = Time.adaptive_fps 300 in

  let camel_dir_ref = ref (0, 0) in

  (* TODO: add trailing effect behind camel (can be an effect )*)
  (* TODO: fix error with delayed arrow movement *)
  let rec mainloop e =
    let camel = !camel_ref in
    let camel_dir = !camel_dir_ref in
    let humans = !human_ref_lst in
    let map = !map_ref in
    let move_check dir = camel_dir_ref := dir in

    (* if paused, then disable the below internally, not the whole match body *)
    (if Sdl.poll_event (Some e) then
     match current_state state with
     | Active -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
             move_check (0, -1)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
             move_check (0, 1)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
             move_check (1, 0);
             camel_facing := false
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
             move_check (-1, 0);
             camel_facing := true
         | `Key_down
           when List.mem
                  Sdl.Event.(get e keyboard_keycode)
                  [ Sdl.K.r; Sdl.K.space ] ->
             camel_dir_ref := (0, 0);
             reset_game (int 10000)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.p ->
             change_state state Pause
         | _ -> ())
     | Inactive -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.s ->
             camel_dir_ref := (0, 0);
             reset_game (int 10000);
             let th_game : Thread.t = Thread.create change_game_board () in
             Thread.join th_game;
             (* let th : Thread.t = Thread.create Sync.push change_board in *)
             let make_window () = make_sdl_windows ~windows:[ win ] !board in
             let th_window : Thread.t = Thread.create make_window () in
             Thread.join th_window
         | _ -> ())
     | Pause -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.p ->
             change_state state Active
         | _ -> ())
     | Win -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.r ->
             (* let th : Thread.t = Thread.create Sync.push change_board in *)
             let th : Thread.t = Thread.create change_game_board () in
             Thread.join th;
             let make_window () = make_sdl_windows ~windows:[ win ] !board in
             let th2 : Thread.t = Thread.create make_window () in
             Thread.join th2;
             reset_game (int 100000)
         | _ -> ())
     | Lose -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.r ->
             (* print_endline "restart"; *)

             (* let th : Thread.t = Thread.create Sync.push change_board in *)
             let change_greeting_board () =
               board := make_greeting_board;
               change_state state Inactive;
               reset_game (Random.int 100);
               make_sdl_windows ~windows:[ win ] !board
             in
             let th = Thread.create change_greeting_board () in
             Thread.join th
         | _ -> ())
     | _ -> ());

    Draw.set_color renderer bg;

    go (Sdl.render_clear renderer);
    (* replace render_fill_rect with rendering an image of a camel *)
    refresh_custom_windows !board;

    if
      not (one_step true (start_fps, fps) !board)
      (* one_step returns true if fps was executed *)
    then fps ()
    else fps ();

    (* checks item expiration *)
    check_item_expiration map_ref;

    (* TODO: testing states *)
    "time: " ^ string_of_int !state_time ^ ", score: "
    ^ string_of_int !state_score ^ ", lives: " ^ string_of_int !state_lives
    ^ ", num coins:"
    ^ string_of_int !state_num_coins
    |> print_endline;

    W.set_text score_w ("Score: " ^ string_of_int !state_score);
    (* item rendering *)
    (* TODO @Yaqi: replace rectangles with images *)
    let item_list = get_items map in
    let new_rect (w, h) (x, y) = Sdl.Rect.create ~x ~y ~w ~h in
    Draw.set_color renderer (100, 200, 200, 255);

    if current_state state = Active || current_state state = Pause then (
      List.iter
        (fun ((x, y), item_ref) ->
          let item = !item_ref in
          let size = Item.size item in
          let x, y =
            match Item.item_type item with
            | SmallCoin ->
                let x_u, y_u = unit_size () in
                (x + (x_u / 4), y + (y_u / 4))
            | _ -> (x, y)
          in
          let x_shift, y_shift = Item.shift item in
          let loc = (x + x_shift, y + y_shift) in
          let texture =
            match Item.item_type item with
            | SmallCoin | BigCoin -> coin_texture
            | Coins -> coin_pile_texture
            | Speed -> speed_texture
            | Sand -> sand_texture
            | Phase -> phase_texture
            | Cactus -> cactus_texture
            | Life -> heart_texture
            | Time -> time_texture
            | Invincible -> shield_texture
          in

          Sdl.render_copy_ex
            ?dst:(Some (new_rect size loc))
            renderer texture 0. None (Item.flip item)
          |> ignore)
        item_list;

      (* Camel and human rendering happens after fps so that they are on top of
         the map. *)

      (* camel rendering logic *)
      let x_c, y_c = Camel.pos camel in
      let w, h = Camel.size camel in
      let flip = if !camel_facing then Sdl.Flip.horizontal else Sdl.Flip.none in
      let blinkPeriod = 20 in
      if
        (not state_camel.invincible)
        || !state_time mod blinkPeriod < blinkPeriod / 2
      then
        go
          (Sdl.render_copy_ex
             ?dst:(Some (new_rect (w, h) (x_c, y_c)))
             renderer camel_texture 0. None flip);

      (* check update on camel state *)
      let camel_spd =
        (if state_camel.doubleSpeed then 2 else 1) * Camel.speed camel
      in
      let camel_period = 30 / camel_spd in
      if camel_spd <> 0 && !state_time mod camel_period = 0 then
        Camel.move camel_ref map_ref camel_dir (fun () -> ());

      (* human rendering logic *)
      List.iteri
        (fun i human ->
          (* abstract into a function, then use this in tween *)
          let x_h, y_h = Human.pos !human in
          let w, h = Human.size !human in
          let render_human () =
            let x, y = Human.pos !human in
            (* check for collision between camel and human *)
            let thres = 100 in
            let dist =
              let xDiff = x_h - x_c in
              let yDiff = y_h - y_c in
              (xDiff * xDiff) + (yDiff * yDiff)
            in
            (* collision effect *)
            if dist < thres && current_state state = Active then
              if state_human.scared then begin
                (* TODO: add notify score effect? *)
                state_score :=
                  !state_score + if state_camel.doubleCoin then 20 else 10;
                state_human.doubleSpeed <- true;
                (* set false after 5 seconds *)
                Item.make_effect 5.
                  (fun () -> state_human.doubleSpeed <- false)
                  ()
              end
              else if not state_camel.invincible then begin
                state_lives := !state_lives - 1;
                (if !state_lives = 0 then
                 let change_lose_board () =
                   W.set_text final_score
                     ("Your Final Score is : " ^ string_of_int !state_score);
                   board := make_lose_board;
                   change_state state Lose;
                   make_sdl_windows ~windows:[ win ] !board
                 in
                 let th_lose : Thread.t = Thread.create change_lose_board () in
                 Thread.join th_lose);

                state_camel.invincible <- true;
                Item.make_effect 5.
                  (fun () -> state_camel.invincible <- false)
                  ()
              end;
            go
              (Sdl.render_copy
                 ?dst:(Some (Sdl.Rect.create ~x ~y ~w ~h))
                 renderer human_texture)
          in
          let human_spd =
            (* human speed increases with score *)
            (Human.speed !human |> float_of_int)
            *. (1. +. ((!state_score |> float_of_int) /. 200.))
            |> int_of_float
          in
          (* check if state_human.halfSpeed or doubleSpeed is true *)
          let human_spd =
            (* maximum speed set to 10 *)
            min 10
              (if state_human.doubleSpeed then 2 * human_spd
              else if state_human.halfSpeed then max (human_spd / 2) 1
              else human_spd)
          in
          let human_period = 30 / human_spd in
          (if
           human_spd <> 0
           && !state_time mod human_period = 0
           && Time.now () - !time_ref > (i * 10000) + 1000
           && (x_h <> x_c || y_h <> y_c)
           && current_state state = Active
          then
           let dest =
             if state_human.scared then human_init_pos map i else (x_c, y_c)
           in
           let dir = get_path_dir map (x_h, y_h) dest in
           Human.move human map_ref dir render_human);
          render_human ())
        humans;
      if !state_lives > 0 then
        go
          (Sdl.render_copy
             ?dst:(Some (Sdl.Rect.create ~x:850 ~y:150 ~w:40 ~h:40))
             renderer camel_texture);
      if !state_lives > 1 then
        go
          (Sdl.render_copy
             ?dst:(Some (Sdl.Rect.create ~x:890 ~y:150 ~w:40 ~h:40))
             renderer camel_texture);
      if !state_lives > 2 then
        go
          (Sdl.render_copy
             ?dst:(Some (Sdl.Rect.create ~x:930 ~y:150 ~w:40 ~h:40))
             renderer camel_texture);
      if !state_num_coins = 0 then
        let change_win_board () =
          W.set_text final_score
            ("Congratulation! Your Final Score is : "
           ^ string_of_int !state_score);
          board := make_win_board;
          change_state state Win;
          make_sdl_windows ~windows:[ win ] !board
        in
        let th_lose : Thread.t = Thread.create change_win_board () in
        Thread.join th_lose);

    if current_state state = Active then begin
      (* add possible item to map_ref *)
      if float 1. > 0.7 then add_item map_ref;
      (* animate items *)
      animate_items (item_list |> List.map (fun (_, item_ref) -> item_ref));
      (* update time counter *)
      incr state_time;
      W.set_text display_time
        ("Timer: " ^ string_of_int ((!state_end_time - !state_time) / 100));
      if !state_time >= !state_end_time then begin
        W.set_text final_score
          ("Time's up! Your Final Score is : " ^ string_of_int !state_score);

        let change_lose_board () =
          board := make_lose_board;
          change_state state Lose
        in
        let th3 : Thread.t = Thread.create change_lose_board () in
        Thread.join th3;
        let make_window () = make_sdl_windows ~windows:[ win ] !board in
        let th2 : Thread.t = Thread.create make_window () in
        Thread.join th2
      end
    end;

    if current_state state = Pause then (
      Draw.set_color renderer (255, 255, 255, 100);
      go
        (Sdl.render_fill_rect renderer
           (Some (Sdl.Rect.create ~x:0 ~y:0 ~w:800 ~h:800))));

    Sdl.render_present renderer;
    mainloop e
  in
  let e = Sdl.Event.create () in
  start_fps ();
  (*TODO: change this line back*)
  (* let () = try mainloop e with _ -> exit 0 in *)
  let () = mainloop e in
  Sdl.destroy_window win;
  Draw.quit ()

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget
   events) *)
let greeting =
  reset_game (int 5000);
  main ()
