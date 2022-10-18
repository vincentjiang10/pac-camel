type t = {
  src : string;
  effect : unit;
  animate : unit;
}

let effect t = t.effect
let animate t = t.animate

(* TODO: (optional) a path predictor item that predicts how each of the humans
   might reach the camel? *)
