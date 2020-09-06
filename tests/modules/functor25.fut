module mk_m1 (R: {}) (S: {type s}) = {
  let f1 (k_m: S.s) = 0i32
}

module mk_m2 (S: {type s}) = {
  module solve = mk_m1 {} S

  let f2 (k_m: S.s) = solve.f1 k_m
}

module mk_sm (R: {}) = {
  type s = {}
}

module sm = mk_sm {}
module m2 = mk_m2 sm

let main = m2.f2
