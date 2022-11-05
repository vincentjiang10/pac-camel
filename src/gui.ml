open Bogue
open Main
open Pacmap
open Tsdl
open Utils
open Random
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
let canvas = W.sdl_area ~w:800 ~h:800 ()
let canvas_l = L.resident ~x:0 ~y:0 canvas
let sdl_area = W.get_sdl_area canvas

(* reference to map *)
let map_ref = ref (gen_map (int 500) sdl_area)

(* reference to camel *)
let camel_ref = ref (Camel.init !map_ref "assets/images/camel-cartoon.png")

let reset_map (seed : int) =
  (* reset canvas *)
  Sdl_area.clear sdl_area;
  map_ref := gen_map seed sdl_area;
  camel_ref := Camel.init !map_ref "assets/images/camel-cartoon.png";
  draw_map sdl_area !map_ref

(* sets up the game *)
let reset_game seed = reset_map seed
let bg = (255, 255, 255, 255)

let make_board () =
  (* set what to be drawn *)
  (* TODO @GUI: clicking on widgets do not work: try to fix *)
  let layout = L.superpose [ canvas_l ] in

  (* TODO @GUI: fix widget dimensions ans positions *)
  (* TODO @GUI: fix error where initial click does not generate correct map *)
  (* action to be connected to start button *)
  let start_action _ _ _ =
    (* reset_game (int 10000); *)
    L.set_rooms layout [ start_button_l; canvas_l ]
    (* this line replace current layout with an empty list, add widgets (to be
       drawn after start) in this list e.g. map, camel, human etc. *)
  in
  (* connect action to button. Triggered when button is pushed*)
  (* let c = W.connect start_button_w start_button_w start_action T.buttons_down
     in *)
  (* set up board *)
  of_layout layout

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
  let camel_texture =
    let camel_surface =
      Tsdl_image.Image.load "assets/images/camel-cartoon.png"
    in
    let t = create_texture_from_surface renderer (go camel_surface) in
    go t
  in

  (* very important: set blend mode: *)
  go (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);

  (* go (Sdl.set_texture_blend_mode camel_texture Sdl.Blend.mode_none); *)
  Draw.set_color renderer bg;
  go (Sdl.render_clear renderer);

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
     | `Key_down
       when List.mem Sdl.Event.(get e keyboard_keycode) [ Sdl.K.r; Sdl.K.space ]
       ->
         print_endline "restart";
         reset_game (int 10000)
     | _ -> ());

    Draw.set_color renderer bg;

    go (Sdl.render_clear renderer);
    Draw.set_color renderer (100, 200, 200, 255);
    let x, y = Camel.get_pos camel in
    let w, h = Camel.get_size camel in

    (* replace render_fill_rect with rendering an image of a camel *)
    refresh_custom_windows board;
    go (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)));

    if
      not (one_step true (start_fps, fps) board)
      (* one_step returns true if fps was executed *)
    then fps ()
    else fps ();
    go
      (Sdl.render_copy
         ?dst:(Some (Sdl.Rect.create ~x ~y ~w ~h))
         renderer camel_texture);
    Sdl.render_present renderer;
    mainloop e
  in

  let e = Sdl.Event.create () in
  start_fps ();
  let () = try mainloop e with _ -> exit 0 in
  Sdl.destroy_window win;
  Draw.quit ()

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget
   events) *)
let greeting = main ()
