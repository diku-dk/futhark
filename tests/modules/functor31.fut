-- Distilled from #2273. The problem was that we destroyed type annotations when
-- generating new names for the result of applying a parameterised module.
-- ==
-- input {} output { 0i64 1i64 }

module fraction = {
  type t = (i64, i64)
  def sub (a, b) (c, d) : t = (a * d - b * c, b * d)
  def zero : t = (0, 1)
}

module SI (_v: {}) = {
  module localfraction = fraction

  type unit [s] [s_d] =
    [0][s][s_d]()

  def unit (s: fraction.t) : unit [s.0] [s.1] =
    []

  def div [s0] [s_d0] [s1] [s_d1]
          (_: unit [s0] [s_d0])
          (_: unit [s1] [s_d1]) =
    unit (fraction.sub (s0, s_d0) (s1, s_d1))

  def m = unit fraction.zero
}

module M = SI {}

entry main = let [x][y] (_: [0][x][y]()) = M.div M.m M.m in (x, y)
