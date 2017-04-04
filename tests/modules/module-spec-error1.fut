-- A parametric module does not match a module spec.
-- ==
-- error: parametric

module type MT = {
  module M: {val x: i32}
}

module M1: MT = {
  module M(P: {val y:i32}) = { let x = P.y }
}

let main() = M1.M.x
