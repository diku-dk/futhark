import "../lib/github.com/diku-dk/cpprandom/random"

type pair = (i32, i32)

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

entry gen_pair (size: i64) (seed: i32) : pair =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (rng1, x) = rand_i32.rand (i32.lowest, i32.highest) rng0
  let (_, y) = rand_i32.rand (-i32.i64 size, i32.i64 size) rng1
  in if x > y then (x, y) else (y + 1i32, y)

-- This is intentionally false for generated values.
-- It fails exactly when x > y and y > 0.
let bad_pair ((x, y): pair) : bool =
  not (x > y && y > 0)

--==
-- property: prop_pair_backtrack



#[prop(gen(gen_pair), shrink(shrink_pair))]
entry prop_pair_backtrack (p: pair) : bool =
  bad_pair p

let step_towards_zero (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_pair ((x, y): pair) (tactic: i32) : (pair, i8) =
  if tactic == 0 then
    let x' = step_towards_zero x
    in ((x', y), i8.bool (x' == x))
  else if tactic == 1 then
    let y' = step_towards_zero y
    in ((x, y'), i8.bool (y' == y))
  else
    ((x, y), 2)