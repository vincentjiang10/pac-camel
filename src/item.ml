type t = {
  probabilty : float;
  period : int;
  src : string;
  effect : unit;
  animate : unit;
}

let period t = t.period
let probability t = t.probabilty
let src t = t.src
let effect t = t.effect
let animate t = t.animate

(* Power-up creation *)

(* Doubles the value of coins earned *)
let coinsItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

(* Doubles speed *)
let speedItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

(* Show trajectory of humans (in their respective colors) *)
let trajectoryItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

(* Trails or places bomb/quicksand that stuns ghosts for a few seconds *)
let sandItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

(* Phase through walls *)
let phaseItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }

(* Cactus (scares away humans) *)
let cactusItem () =
  { probabilty = 0.01; period = 10; src = ""; effect = (); animate = () }
