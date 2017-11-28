-- Another Parametric module operating on a module with nested
-- modules.
-- ==
-- input { 2 } output { 3 }

module type to_i32 = {
  type t
  val to_i32: t -> i32
}

module i32 = {
  type t = i32
  let to_i32 (x: i32) = x
}

module type engine = {
  module int: to_i32
  val min: int.t
}

module an_engine = {
  module int = i32
  let min = 1
}

module mk_has_y (E: engine) = {
  let y = E.int.to_i32 E.min
}

module m1 = mk_has_y an_engine

let main (x: i32) = m1.y + x
