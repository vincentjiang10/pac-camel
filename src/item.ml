type t = {
  src : string;
  effect : unit;
  animate : unit;
}

let effect t = t.effect
let animate t = t.animate