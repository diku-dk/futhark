import "../libraries/shrinkers/integerShrinker"
import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- u32 tests ------------------
-- Uniform u32 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_u32 = uniform_int_distribution u32 u32 rng_engine
module shrink_u32 = integerlShrinkers u32

entry gen_simple (size: i64) (seed: i32) : u32 =
  let rng0 = rng_engine.rng_from_seed [seed]
--   let (_, x) = rand_u32.rand (u32.lowest, u32.highest) rng0
  let (_, x) = rand_u32.rand (0u32, u32.i64 size) rng0
  in x

let simple_succ (x: u32) : u32 =
  u32.abs x

-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: u32) : bool =
    simple_succ x == u32.abs x

let simple_fail (x: u32) : u32 =
  x-(u32.highest)/2

-- should fail on all numbers under half of the maximum value
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: u32) : bool =
    simple_fail x < x

entry shrink_simple (x: u32) (random: i32) : u32 =
  shrink_u32.shrinker x random
