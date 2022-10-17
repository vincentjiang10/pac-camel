open Game
open Gui
open Pacmap

let playOrPause () = ()

(* TODO @GUI: add to this function, which should initialize gui widgets (be
   prepared to take in functions that should be called based on widget events)*)
let startGUI () = greeting
let main () = startGUI ()

(* Execute game *)
let () = main ()