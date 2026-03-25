-- Regression test for #2407. Defunctorisation of higher-order modules
-- (functors as arguments) used to enter an infinite loop due to cyclic
-- substitutions being created when matching two functor module types.

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
  local module Triangle = P M_A
}

module R = PM M_B
