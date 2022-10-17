open Bogue
open Main
open Pacmap
module W = Widget
module L = Layout
module T = Trigger

(* SETUP *)
(* WIDGETS*)
let start_title_w =
  W.label ~size:32
    ~fg:Draw.(opaque (find_color "firebrick"))
    "The Pac-Camel Game"

let start_button_w = W.button "Start the Game"

(*LAYOUT*)
let start_title_l = L.resident start_title_w ~y:2
let start_button_l = L.resident ~x:130 ~y:35 ~w:50 ~h:2 start_button_w

(* TODO @GUI (post-MS2): a few more widgets to implement - a play/pause button -
   a reset button (has the same action as start button (just with the text
   changed) - a text field to generate a map with a specific seed -> a button to
   apply (or just reset) *)

(* TODO @GUI (post-MS2): fix canvas margins; currently resizing windows causes
   undesirable behavior; perhaps set a minimum window dimension *)
let canvas = W.sdl_area ~w:600 ~h:600 ()
let canvas_l = L.resident ~x:200 ~y:40 canvas

(* TODO: temporary -> will need to transition to camel code in camel.ml *)
let camel_image =
  let src = W.image ~w:40 ~h:40 "assets/images/camel-cartoon.png" in
  L.resident ~x:500 ~y:300 src

let sdl_area = W.get_sdl_area canvas

let reset_map (seed : int) =
  (* reset canvas *)
  let _ = Sdl_area.clear sdl_area in
  draw_map sdl_area (gen_map seed)

(* sets up the game *)
let reset_game seed = reset_map seed

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget events)*)
let greeting =
  (* set what to be drawn *)
  let layout = L.superpose [ start_title_l; start_button_l ] in
  (* set default window size *)
  let _ = Timeout.add 1 (fun () -> L.set_size layout (1000, 800)) in

  (* TODO @GUI: fix error where initial click does not generate correct map *)
  (* action to be connected to start button *)
  let start_action _ _ _ =
    reset_game (Random.int 400);
    L.set_rooms layout [ start_button_l; canvas_l; camel_image ]
    (* this line replace current layout with an empty list, add widgets (to be
       drawn after start) in this list e.g. map, camel, human etc. *)
  in
  (* connect action to button. Triggered when button is pushed*)
  let c = W.connect start_button_w start_button_w start_action T.buttons_down in

  (* set up board *)
  let board = of_layout ~connections:[ c ] layout in
  run board