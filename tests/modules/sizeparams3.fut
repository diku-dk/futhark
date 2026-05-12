-- More size parameters in a parametric type.
-- ==
-- input { 1 1 } output { [0] }
-- input { 1 2 } error:

module PM (P: {type vec [n] val mk : (n: i64) -> vec [n]}) = {
  def can_be_bad (n: i64) (x: i64) = P.mk x :> P.vec [n]
}

module intmat = PM {
  type vec [n] = [n]i32
  def mk (n: i64) = replicate n 0
}

def main (n: i32) (x: i32) = intmat.can_be_bad (i64.i32 n) (i64.i32 x)
