-- Can we nest a parametric module inside another parametric module?
-- ==
-- input { 3 } output { 6 }

module type MT1 = {
  val f: i32 -> i32 -> i32
}

module type MT2 = {
  val g: i32 -> i32
}

module M = {
  module T1(P1: MT1) = {
    module T2(P2: MT1) = {
      fun g(x: i32) = P2.f (P1.f x x) x
    }
  }
}

module T1a = M.T1({fun f (x: i32) (y: i32) = x + y})
module T = T1a.T2({fun f (x: i32) (y: i32) = x * y})

fun main (x: i32) = T.g x
