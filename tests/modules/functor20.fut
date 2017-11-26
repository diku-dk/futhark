-- Parametric module operating on a module with nested modules.
-- ==
-- input { 2 } output { 3 }

module type integral = {
  type t
  val frob: t -> t
}

module quux = {
  type t = i32
  let frob (x: i32) = x + 1
}

module type has_int = {
  module int: integral
}

module mk_has_int (T: integral): has_int with int.t = T.t = {
  module int = T
}

module has_quux = mk_has_int quux

module frob_int (E: has_int) = {
  let really_frob (x: E.int.t) = E.int.frob x
}

module m = frob_int has_quux

let main (x: i32) = m.really_frob x
