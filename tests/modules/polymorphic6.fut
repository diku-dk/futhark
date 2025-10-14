-- Removing polymorphism with an ascription, but higher order!
-- ==
-- input { 2 } output { 2 }

module mk_m
  (P: {
    type t
    val f : t -> i32
  }) = {
  def g (x: P.t) = P.f x
}

module m = mk_m {
  type t = (i32, i32)
  def f (x, _) = x
}

def main (x: i32) =
  m.g (x, x)
