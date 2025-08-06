-- Size parameters in a parametric type.
-- ==
-- input { 1 2 } output { [[0,0]] }

module PM (P: {type vec [n] val mk_a : (n: i64) -> vec [n]}) = {
  def mk_b (m: i64) (n: i64) : [m](P.vec [n]) = replicate m (P.mk_a n)
}

module intmat = PM {
  type vec [n] = [n]i32
  def mk_a (n: i64) = replicate n 0
}

def main (m: i32) (n: i32) = intmat.mk_b (i64.i32 m) (i64.i32 n)
