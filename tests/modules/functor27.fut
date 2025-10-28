-- Ensuring the right references to sizes even through tricky
-- indirections.

module mk (P: {val n : i64})
  : {
      val mk 't : t -> [P.n]t
    } = {
  def mk = replicate P.n
}

module mk2 (P: {val n : i64}) = {
  module m = mk P
  def f (t: bool) = m.mk t
}

module m = mk2 {def n = 10i64}

def main = m.f
