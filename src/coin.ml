type t = {
  bSrc : string;
  sSrc : string;
  bVal : int;
  sVal : int;
}

let big_coin_image t = t.bSrc
let small_coin_image t = t.sSrc
let big_coin_val t = t.bVal
let small_coin_val t = t.sVal
