-- A module spec in a module type.
-- ==
-- input {} output { 2 }

module type MT = {
  module M: {val x: i32}
}

module M1: MT = {
  module M = { let x = 2 }
}

fun main() = M1.M.x
