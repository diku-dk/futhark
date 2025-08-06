-- Signature matching with a single abstract type.
-- ==
-- input { [1,2,3] [4,5,6] }
-- output { 6 15 }

module type SIG = {
  type t

  val inject : i32 -> i32 -> t
  val extract : t -> (i32, i32)
  val f [n] : [n]t -> t
}

module Struct : SIG = {
  type t = (i32, i32)

  def x : (i32, i32) = (2, 2)

  def inject (x: i32) (y: i32) : t = (x, y)
  def extract (v: t) : t = v
  def f (as: []t) : t = reduce (\(a, b) (c, d) -> (a + c, b + d)) (0, 0) as
}

def main (xs: []i32) (ys: []i32) : (i32, i32) =
  Struct.extract (Struct.f (map2 Struct.inject xs ys))
