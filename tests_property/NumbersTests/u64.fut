import "../libraries/shrinkers/integerShrinker"
import "../lib/github.com/diku-dk/cpprandom/random"

------------------ u64 tests ------------------
-- Uniform u64 distribution using minstd_rand (u32 engine) underneath.
module rng_engine_u64 = minstd_rand
module rand_u64 = uniform_int_distribution u64 u32 rng_engine_u64
module shrink_u64 = integerlShrinkers u64

entry gen_simple (size: i64) (seed: i32) : u64 =
  let rng0 = rng_engine_u64.rng_from_seed [seed]
--   let (_, x) = rand_u64.rand (u64.lowest, u64.highest) rng0
  let (_, x) = rand_u64.rand (0, u64.i64 size) rng0
  in x

let simple_succ (x: u64) : u64 =
  u64.abs x

-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: u64) : bool =
    simple_succ x == u64.abs x

let simple_fail (x: u64) : u64 =
  x-(u64.highest)/2

-- should fail on all numbers under half of the maximum value
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: u64) : bool =
    simple_fail x < x

entry shrink_simple (x: u64) (random: i32) : u64 =
  shrink_u64.shrinker x random
