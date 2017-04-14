-- A module with abstract types containing size parameters.
-- ==
-- input { 3 } output { 3 }

module type MT = {
  type intvec[n]

  val singleton: i32 -> intvec [1]
  val first: intvec [] -> i32
}

module M0: MT = {
  type intvec [n] = [n]i32
  let singleton (x: i32) = [x]
  let first (x: intvec[#n]) = x[0]
}

fun main(x: i32): i32 =
  let y: M0.intvec[1] = M0.singleton x
  in M0.first y
