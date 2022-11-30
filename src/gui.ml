open Bogue
open Main
open Pacmap
open Movable
open Camel
open Human
open Tsdl
open Utils
open Random
open Sdl
open GameState
module W = Widget
module L = Layout
module T = Trigger

(* SETUP *)

let width = 800
let height = 800

(* WIDGETS*)
let start_title_w =
  W.label ~size:32
    ~fg:Draw.(opaque (find_color "firebrick"))
    "The Pac-Camel Game"

let start_button_w = W.button "Press [s] to Start the Game"

let thick_grey_line =
  Style.mk_line ~color:Draw.(opaque black) ~width:3 ~style:Solid ()

let round_blue_box =
  let open Style in
  let border = mk_border ~radius:2 thick_grey_line in
  create ~border ~background:(color_bg Draw.(opaque @@ find_color "black")) ()

let bg_off = Style.color_bg Draw.none
let bg_over = Some (Style.opaque_bg Draw.grey)
let fg = Draw.(opaque black)

(*LAYOUT*)

(* TODO @GUI (optional): animate in the title + add hovering to buttons upon
   user mouse hover event *)
(* TODO (add keyboard options for quit (Q), play/pause (spacebar or P), and H (H
   or ?); these would trigger events that the buttons that they correspond to
   trigger *)
let start_title_l = L.resident start_title_w ~y:2
let start_button_l = L.resident ~x:30 ~y:35 ~w:250 ~h:2 start_button_w

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

(* TODO @GUI (post-MS2): fix canvas margins; currently resizing windows causes
   undesirable behavior; perhaps set a minimum window dimension *)
let canvas = W.sdl_area ~w:800 ~h:800 ~style:round_blue_box ()
let canvas_l = L.resident ~x:0 ~y:0 canvas
let start = W.image "assets/images/start.png"
let start_l = L.resident start
let life1 = W.image "assets/images/camel.png" ~w:10 ~h:10
let life2 = W.image "assets/images/camel.png" ~w:10 ~h:10 ~angle:10.0
(* ~bg:Draw.(opaque black) *)

let life3 = W.image "assets/images/camel.png" ~w:10 ~h:10 ~angle:20.0
let score = ref 0

let score_w =
  W.label
    ("Score: " ^ string_of_int !score)
    ~fg:Draw.(opaque (find_color "black"))

let score_l = L.resident score_w ~x:870 ~w:100 ~h:100 ~y:200

let pause_button =
  W.button "Pause" ~bg_off (* ~bg_on *)
    ~bg_over ~kind:Switch
    ~label_on:(Label.icon ~fg "pause-circle")
    ~label_off:(Label.icon ~fg:Draw.(lighter (lighter fg)) "pause-circle")

let pause_button_l = L.resident pause_button ~x:950 ~y:20 ~w:50 ~h:30
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

let human_ref_lst = ref (human_inits [] 4)
let game_state = GameState.init camel_ref human_ref_lst (ref []) map_ref

let reset_map (seed : int) =
  (* reset canvas *)
  Sdl_area.clear sdl_area;
  map_ref := gen_map seed sdl_area;
  camel_ref := Camel.init !map_ref "assets/images/camel-cartoon.png";
  human_ref_lst := human_inits [] 4;
  draw_map sdl_area !map_ref

(* sets up the game *)
let reset_game seed =
  reset_map seed;
  GameState.reset game_state

let bg = (255, 255, 255, 255)

let make_greeting_board =
  let greeting_layout = L.superpose [ start_l ] in
  L.set_width greeting_layout 800;
  L.set_height greeting_layout 800;

  of_layout greeting_layout

let make_game_board =
  let lives_l = L.flat_of_w [ life1; life2; life3 ] in
  L.set_height lives_l 60;
  L.set_width lives_l 150;
  L.setx lives_l 850;
  L.sety lives_l 150;
  (* set what to be drawn *)
  (* TODO @GUI: clicking on widgets do not work: try to fix *)
  let layout =
    L.superpose [ canvas_l; score_l; pause_button_l; lives_l; Space.hfill () ]
  in
  L.set_width layout 481;
  L.set_height layout 385;
  L.fix_content layout;
  let button_connection =
    W.connect pause_button pause_button
      (fun _ _ _ -> change_state game_state Active)
      T.buttons_down
  in
  of_layout ~connections:[ button_connection ] layout

let board = ref make_greeting_board

let main () =
  let open Sdl in
  Sys.catch_break true;
  go (Sdl.init Sdl.Init.video);
  let win =
    go
      (Sdl.create_window ~w:1000 ~h:800 "Pac-Camel Game"
         Sdl.Window.(shown + popup_menu))
  in

  let renderer = go (Sdl.create_renderer win) in
  let camel_texture =
    let camel_surface = Tsdl_image.Image.load (Camel.src !camel_ref) in
    let t = create_texture_from_surface renderer (go camel_surface) in
    go t
  in

  let human_texture =
    let human_surface =
      Tsdl_image.Image.load (Human.src !(List.hd !human_ref_lst))
    in
    let t = create_texture_from_surface renderer (go human_surface) in
    go t
  in
  (* very important: set blend mode: *)
  go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);

  Draw.set_color renderer bg;
  go (Sdl.render_clear renderer);

  (* let show_gui = ref true in *)
  make_sdl_windows ~windows:[ win ] !board;
  let start_fps, fps = Time.adaptive_fps 10 in

  let camel_dir_ref = ref (0, 0) in
  let auto = ref true in

  (* TODO: add trailing effect behind camel (can be an effect )*)
  (* TODO: maybe let the camel always be going in a direction, which can be
     changed on key input *)
  let rec mainloop e =
    let camel = !camel_ref in
    let camel_dir = !camel_dir_ref in
    let humans = !human_ref_lst in
    let map = !map_ref in
    let camel_speed = Camel.speed camel in
    let human_speed = Human.speed !(List.nth humans 0) in
    let move_check dir =
      (* if camel_dir = dir then auto := true else begin auto := false;
         Camel.move camel_ref map dir; camel_dir_ref := dir end *)
      camel_dir_ref := dir
    in
    (if Sdl.poll_event (Some e) then
     match current_state game_state with
     | Active -> (
         (* if W.get_state pause_button then ( print_endline " pause";
            change_state game_state Pause) else *)
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
             move_check (0, -camel_speed)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
             move_check (0, camel_speed)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
             move_check (camel_speed, 0)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
             move_check (-camel_speed, 0)
         | `Key_down
           when List.mem
                  Sdl.Event.(get e keyboard_keycode)
                  [ Sdl.K.r; Sdl.K.space ] ->
             print_endline "restart";
             reset_game (int 10000)
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.p ->
             change_state game_state Pause
         | _ -> ())
     | Inactive -> (
         match Trigger.event_kind e with
         | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.s ->
             let change_board () =
               board := make_game_board;
               GameState.change_state game_state Active
             in

             let th : Thread.t = Thread.create change_board () in
             Thread.join th;

             let make_window () = make_sdl_windows ~windows:[ win ] !board in
             let th2 : Thread.t = Thread.create make_window () in
             Thread.join th2;
             reset_game (int 10000)
         | _ -> ())
     | Pause ->
         if not (W.get_state pause_button) then (
           print_endline "start";
           change_state game_state Active)
     | _ -> ());

    (* TODO: implement auto camel movement in the direction of !camel_dir? *)
    Draw.set_color renderer bg;
    if !auto then Camel.move camel_ref map camel_dir;

    go (Sdl.render_clear renderer);
    Draw.set_color renderer (100, 200, 200, 255);
    let x_c, y_c = Camel.pos camel in
    let w, h = Camel.size camel in
    refresh_custom_windows !board;

    (let render_rect ~x ~y ~w ~h =
       go (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)))
     in

     (* TODO: implement a timer that brings out the humans one at a time *)
     if
       not (one_step true (start_fps, fps) !board)
       (* one_step returns true if fps was executed *)
     then fps ()
     else fps ();

     (* Camel and human rendering happens after fps so that they are on top of
        the map. *)
     if GameState.current_state game_state != Inactive then (
       List.iteri
         (fun i human ->
           let x_h, y_h = Human.pos !human in
           let w, h = Human.size !human in
           go
             (Sdl.render_copy
                ?dst:(Some (Sdl.Rect.create ~x:x_h ~y:y_h ~w ~h))
                renderer human_texture);
           if current_state game_state = Active then
             try
               if (* experimental *)
                  float 1. > 0.5 then
                 let scale k (x, y) = (k * x, k * y) in
                 Human.move human map
                   (get_path_dir map (x_h, y_h) (x_c, y_c) |> scale human_speed)
             with _ -> ())
         humans;
       go
         (Sdl.render_copy
            ?dst:(Some (Sdl.Rect.create ~x:x_c ~y:y_c ~w ~h))
            renderer camel_texture));
     if current_state game_state = Pause then (
       (* render transulent screen above*)
       Draw.set_color renderer (255, 255, 255, 100);

       render_rect ~x:0 ~y:0 ~w:800 ~h:800));
    Sdl.render_present renderer;
    mainloop e
  in

  let e = Sdl.Event.create () in
  start_fps ();
  let () = try mainloop e with _ -> exit 0 in
  (* let () = mainloop e in *)
  Sdl.destroy_window win;
  Draw.quit ()

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget
   events) *)
let greeting = main ()
