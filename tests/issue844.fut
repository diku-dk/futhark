-- ==
-- error: Consuming.*"xs"

module type mt = {
  type~ t
  val mk : i32 -> *t
  val f : *t -> *t
}

module m : mt = {
  type~ t = []i32
  def mk (x: i32) = [x]
  def f (xs: *[]i32) = xs with [0] = xs[0] + 1
}

def main (x: i32) =
  let f = \xs -> m.f xs
  in f (m.mk x)
