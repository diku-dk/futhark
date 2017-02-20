-- A module spec in a module type, used for a parametric module, with
-- some shadowing too.
-- ==
-- input { 10 } output { 10 }

module PM(P: {type t val x: t module PM: {val f: t -> t}}) = {
  fun iterate(n: i32) = loop (x = P.x) = for i < n do P.PM.f x in x
}

module M = PM({
  type t = i32
  val x = 0
  module PM = {
    fun f(a: i32) = a + 1
  }
})

fun main(n: i32) = M.iterate n
