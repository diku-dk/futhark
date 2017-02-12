-- Complex multiple applications of a parametric module must work.
--
-- ==
-- input { 1 } output { 9 }

module type mt = {
  val f: i32 -> i32
}

module pm1(R: mt): {val g: i32->i32} = {
  fun helper(x: i32) =  R.f (R.f x)
  fun g(x: i32): i32 = helper x
}

module pm2(R: mt) = {
  module tmp = pm1(R)
  fun h(x: i32): i32 = tmp.g (tmp.g x)
}

module m1 = { fun f (x: i32) = x + 1 }

module m2 = pm2(m1)
module m3 = pm2(m1)

fun main(x: i32) = m2.h (m3.h x)
