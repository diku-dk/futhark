-- Instantiating a parametric module twice should go well.
-- ==
-- input { 2 true } output { 2 true }

module f (P: {type t}) = {
  def id (x: P.t) = x
}

module m1 = f ({type t = i32})
module m2 = f ({type t = bool})

def main (x: i32) (y: bool) =
  (m1.id x, m2.id y)
