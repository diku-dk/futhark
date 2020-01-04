-- Signature matching with a single abstract type.
-- ==
-- input { [1,2,3] [4,5,6] }
-- output { 6 15 }

module type SIG = {
  type t

  val inject: i32 -> i32 -> t
  val extract: t -> (i32,i32)
  val f [n]: [n]t -> t
}

module Struct: SIG = {
  type t = (i32,i32)

  let x: (i32, i32) = (2,2)

  let inject (x: i32) (y: i32): t = (x, y)
  let extract (v:t): t = v
  let f (as: []t): t = reduce (\(a,b) (c,d) -> (a+c,b+d)) (0,0) as
}

let main (xs: []i32) (ys: []i32): (i32,i32) =
  Struct.extract (Struct.f (map2 Struct.inject xs ys))
