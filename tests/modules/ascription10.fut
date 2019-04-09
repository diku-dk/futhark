-- An abstract type that is realised by a nested module.

module type number = { type t val i32: i32 -> t }

module has_number: number with t = i32 = { type t = i32 let i32 (x: i32) = x }

module type optimizable = {
  module loss: number
}

module opaque : optimizable = {
  module loss = has_number
}

module mt (optable: optimizable) = {
  module loss = optable.loss
}

module m = mt opaque

let main (x: i32) = m.loss.i32 x
