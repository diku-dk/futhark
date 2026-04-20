import "../libraries/shrinkers/integerShrinker"
import "../lib/github.com/diku-dk/cpprandom/random"

------------------ i64 tests ------------------
-- Uniform i64 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand = uniform_int_distribution i64 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : i64 =
  let rng0 = rng_engine.rng_from_seed [seed]
  -- let (_, x) = rand.rand (i64.lowest, i64.highest) rng0
  let (_, x) = rand.rand (-size, size) rng0
  in x

let simple_succ (x: i64) : i64 =
  i64.abs x

-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: i64) : bool =
    simple_succ x == i64.abs x

let simple_fail (x: i64) : i64 =
  i64.abs x

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: i64) : bool =
    simple_fail x == x

module shrink_i64 = integerlShrinkers i64
entry shrink_simple (x: i64) (random: i32) : i64 =
  shrink_i64.shrinker x random
