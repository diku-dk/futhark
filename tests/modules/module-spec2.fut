-- Higher-order module spec.
-- ==
-- input { 3 } output { 12 }

module type MT1 = {
  val f : i32 -> i32 -> i32
}

module type MT2 = {
  val g : i32 -> i32
}

module type MT3 = {
  module T: MT1 -> MT2
}

module MT3_twice : MT3 = {
  module T (P: MT1) : MT2 = {
    def g (x: i32) = P.f x x
  }
}

module MT1_plus : MT1 = {
  def f (x: i32) (y: i32) = x + y
}

module M = {
  module T (P: MT3) = {
    module P_T_I = P.T MT1_plus

    def g (x: i32) = P_T_I.g x
  }
}

module MT_I = M.T MT3_twice

def main (x: i32) = MT1_plus.f x x + MT_I.g x
