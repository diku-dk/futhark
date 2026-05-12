-- Just one test that actually computes something.
-- ==
-- input { 3 } output { 5 4 }

module type has_t = {
  type^ t
  val v : t
  val ap : t -> i32 -> i32
}

module m1 : has_t = {
  type^ t = i32 -> i32
  def v = (+ 2)
  def ap f x = f x
}

module m2 : has_t = {
  type t = i32
  def v = 1
  def ap = (+)
}

def main (x: i32) = (m1.ap m1.v x, m2.ap m2.v x)
