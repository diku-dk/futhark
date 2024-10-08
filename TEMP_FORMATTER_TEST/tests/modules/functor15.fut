-- Can we nest a parametric module?
-- ==
-- input { 3 } output { 6 }

module type MT1 = {
  val f: i32 -> i32 -> i32
}

module M = {
  module T(P: MT1) = {
    def g(x: i32) = P.f x x
  }
}

module T = M.T({def f (x: i32) (y: i32) = x + y})

def main (x: i32) = T.g x
