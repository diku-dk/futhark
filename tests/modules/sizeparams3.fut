-- More size parameters in a parametric type.
-- ==
-- input { 1 1 } output { [0] }
-- input { 1 2 } error:

module PM(P: { type vec [n] val mk: (n: i32) -> vec [n] }) = {
 let can_be_bad (n: i32) (x: i32) = P.mk x :> P.vec [n]
}

module intmat = PM {
  type vec [n] = [n]i32
  let mk (n: i32) = replicate n 0
}

let main (n: i32) (x: i32) = intmat.can_be_bad n x
