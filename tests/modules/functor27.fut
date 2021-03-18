-- Ensuring the right references to sizes even through tricky
-- indirections.

module mk (P: {val n: i64}) : {
  val mk 't : t -> [P.n]t
} = {
  let mk = replicate P.n
}

module mk2 (P: {val n: i64}) = {
  module m = mk P
  let f (t: bool) = m.mk t
}

module m = mk2 { let n = 10i64 }

let main = m.f
