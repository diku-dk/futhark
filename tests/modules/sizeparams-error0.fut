-- A module with abstract types containing size parameters, instantiated incorrectly.
-- ==
-- error: intvec

module type MT = {
  type intvec[n]

  val singleton: i32 -> intvec [1]
  val first [n]: intvec [n] -> i32
}

module M0: MT = {
  type intvec = [3]i32
  let singleton (x: i32) = [x,x,x]
  let first (x: intvec) = x[0]
}

let main(x: i32): i32 =
  M0.first (M0.singleton x)
