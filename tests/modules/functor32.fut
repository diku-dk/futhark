-- Based on #2407
-- ==
-- input { 2f32 } output { 2f32 }

module type MT_A = {
  type t
}

module type MT_B =
  (V: MT_A)
  -> {
    val f : V.t -> V.t
  }

module M_B : MT_B = \(V: MT_A) ->
  {
    def f (x: V.t) = x
  }

module M_A = {
  type t = f32
}

module PM (P: MT_B) = {
  module Triangle = P M_A
}

module R = PM M_B

entry main (x: f32) = R.Triangle.f x
