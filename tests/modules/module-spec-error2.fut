-- ==
-- error: expecting

module type MT = {
  module M: {type t val x: t val f: t -> t}
}

module M0: MT = {
  module M = {
    type t = i32
    val x = 0
    fun f (y: t) = y + 1
  }
}

module M1: MT = M0

fun main() = M1.M.f (M0.M.x)
