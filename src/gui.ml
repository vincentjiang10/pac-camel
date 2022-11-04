open Bogue
open Main
open Pacmap
open Tsdl
open Utils
open Random
open Camel
open Sdl
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

let start_button_w = W.button "Start the Game"

(*LAYOUT*)

(* TODO @GUI (optional): animate in the title + add hovering to buttons upon
   user mouse hover event *)
(* TODO (add keyboard options for quit (Q), play/pause (spacebar or P), and H (H
   or ?); these would trigger events that the buttons that they correspond to
   trigger *)
let start_title_l = L.resident start_title_w ~y:2
let start_button_l = L.resident ~x:200 ~y:35 ~w:55 ~h:2 start_button_w

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
let canvas = W.sdl_area ~w:500 ~h:500 ()
let canvas_l = L.resident ~x:0 ~y:0 canvas

(* reference to map *)
let map_ref = ref (gen_map (int 500))
let camel_ref = ref (Camel.init !map_ref "assets/images/camel-cartoon.png")
let sdl_area = W.get_sdl_area canvas
let greeting = W.sdl_area ~w:1000 ~h:1000 ()
let greeting_area = W.get_sdl_area greeting

let camel_widget =
  let size = Camel.get_size !camel_ref in
  let width = fst size in
  let height = snd size in
  ref (W.sdl_area ~w:width ~h:height ())

let camel_area = ref (W.get_sdl_area !camel_widget)

let camel_l =
  let pos = get_pos !camel_ref in
  let x_pos = fst pos in
  let y_pos = snd pos in
  ref (L.resident ~x:x_pos ~y:y_pos !camel_widget)

(* let reset_camel () = camel_ref := Camel.init !map_ref
   "assets/images/camel-cartoon.png"; (camel_widget := let size = Camel.get_size
   !camel_ref in let width = fst size in let height = snd size in W.sdl_area
   ?w:width ?h:height ()); camel_area := W.get_sdl_area !camel_widget; camel_l
   := let pos = get_pos !camel_ref in let x_pos = fst pos in let y_pos = snd pos
   in L.resident ~x:x_pos ~y:y_pos !camel_widget *)

let reset_map (seed : int) =
  (* reset canvas *)
  Sdl_area.clear sdl_area;
  map_ref := gen_map seed;
  (* camel := Camel.init !map "assets/images/camel-cartoon.png"; *)
  draw_map sdl_area !map_ref

(* sets up the game *)
let reset_game seed = reset_map seed
let bg = (255, 255, 255, 255)

let make_board () =
  (* set what to be drawn *)
  let layout = L.superpose [ canvas_l; !camel_l ] in
  of_layout layout

let new_rect size x y =
  let w = size in
  let h = size in
  Sdl.Rect.create ~x ~y ~w ~h

let game_start = ref false

let main () =
  let open Sdl in
  Sys.catch_break true;
  go (Sdl.init Sdl.Init.video);
  let win =
    go
      (Sdl.create_window ~w:800 ~h:800 "Pac-Camel Game"
         Sdl.Window.(shown + popup_menu))
  in

  let renderer = go (Sdl.create_renderer win) in

  let greeting_texture =
    let greeting_surface = Tsdl_image.Image.load "assets/images/greeting.png" in
    let t1 = create_texture_from_surface renderer (go greeting_surface) in
    go t1
  in

  let camel_texture =
    let camel_surface =
      Tsdl_image.Image.load "assets/images/camel-cartoon.png"
    in
    let t2 = create_texture_from_surface renderer (go camel_surface) in
    go t2
  in

  (* very important: set blend mode: *)
  go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
  go (Sdl.set_texture_blend_mode greeting_texture Sdl.Blend.mode_none);
  go (Sdl.set_texture_blend_mode camel_texture Sdl.Blend.mode_none);
  Draw.set_color renderer bg;
  go (Sdl.render_clear renderer);
  self_init ();

  (* let show_gui = ref true in *)
  let board = make_board () in
  make_sdl_windows ~windows:[ win ] board;
  let start_fps, fps = Time.adaptive_fps 120 in

  let rec mainloop e =
    let camel = !camel_ref in
    let map = !map_ref in
    let camel_speed = Camel.get_speed camel in
    (if Sdl.poll_event (Some e) then
     match Trigger.event_kind e with
     | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.up ->
         Camel.move camel map (0, -camel_speed)
     | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.down ->
         Camel.move camel map (0, camel_speed)
     | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.right ->
         Camel.move camel map (camel_speed, 0)
     | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.left ->
         Camel.move camel map (-camel_speed, 0)
     | `Key_down when Sdl.Event.(get e keyboard_keycode) = Sdl.K.s ->
         print_endline "start";
         game_start := true (* Sdl_area.set_texture sdl_area greeting_texture *)
     | `Key_down
       when List.mem Sdl.Event.(get e keyboard_keycode) [ Sdl.K.r; Sdl.K.space ]
       ->
         print_endline "restart";
         Sdl_area.clear sdl_area;
         reset_game (int 10000) (* go (render_copy renderer camel_texture) *)
     | _ -> ());

    Draw.set_color renderer bg;

    go (Sdl.render_clear renderer);
    Draw.set_color renderer (100, 200, 200, 255);
    let x, y = Camel.get_pos camel in
    (* replace render_fill_rect with rendering an image of a camel *)
    refresh_custom_windows board;

    L.setx !camel_l x;
    L.sety !camel_l y;

    Sdl_area.set_texture !camel_area camel_texture;

    if !game_start = false then Sdl_area.set_texture sdl_area greeting_texture
    else ();

    (* L.set_show camel_l true; *)

    (* go (Sdl.render_fill_rect renderer (Some (new_rect 20 x y))); *)
    (* go (Sdl.render_copy renderer camel_texture ?dst:(Some (new_rect 20 x y))); *)
    if
      not (one_step true (start_fps, fps) board)
      (* one_step returns true if fps was executed *)
    then fps ()
    else fps ();
    Sdl.render_present renderer;
    mainloop e
  in

  let e = Sdl.Event.create () in
  start_fps ();
  mainloop e
  (*TODO: uncomment*)
(* let () = try mainloop e with _ -> exit 0 in Sdl.destroy_window win; Draw.quit
   () *)

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget
   events) *)
let greeting = main ()
