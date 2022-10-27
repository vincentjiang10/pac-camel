type t = {
  probabilty : float;
  period : int;
  src : string;
  effect : unit;
  animate : unit;
}

let period t = t.period
let probability t = t.probabilty
let effect t = t.effect
let animate t = t.animate

(* TODO: (optional) a path predictor item that predicts how each of the humans
   might reach the camel? *)

(* Power-up options: 
 *  - doubler coin point earn rate
 *  - power-up to show trajectory of ghosts (in color of each ghost)
 *  - trails or places bomb that stuns ghosts for a few seconds
 *  - phase through walls
 *  - 2x speed
 *  - fruit (scares away ghosts)
 *)
