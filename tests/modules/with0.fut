-- Does 'with' work?
-- ==
-- input { 2 } output { 42 }

module type constant = { type t val x: t }

module intconstant: (constant with t = i32) = {
  type t = i32
  val x = 40
}

fun main(y: i32) = intconstant.x + y
