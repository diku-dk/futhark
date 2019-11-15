-- Size parameters in a parametric type.
-- ==
-- input { 1 2 } output { [[0,0]] }

module PM(P: { type vec [n] val mk_a: (n: i32) -> vec [n] }) = {
 let mk_b (m: i32) (n: i32): [m](P.vec [n]) = replicate m (P.mk_a n)
}

module intmat = PM {
  type vec [n] = [n]i32
  let mk_a (n: i32) = replicate n 0
}

let main (m: i32) (n: i32) = intmat.mk_b m n
