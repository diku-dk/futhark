import "../libraries/shrinkers/integerShrinker"
import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- u16 tests ------------------
-- Uniform u16 distribution using minstd_rand (u16 engine) underneath.
module rng_engine = minstd_rand
module rand_u16 = uniform_int_distribution u16 u32 rng_engine
module shrink_u16 = integerlShrinkers u16

entry gen_simple (size: i64) (seed: i32) : u16 =
  let rng0 = rng_engine.rng_from_seed [seed]
--   let (_, x) = rand_u16.rand (u16.lowest, u16.highest) rng0
  let (_, x) = rand_u16.rand (0, u16.i64 (size/2)) rng0
  in x

let simple_succ (x: u16) : u16 =
  u16.abs x

-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: u16) : bool =
    simple_succ x == x

let simple_fail (x: u16) : u16 =
  x-(u16.highest)/2

-- should fail on all numbers under half of the maximum value
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: u16) : bool =
    simple_fail x < x

entry shrink_simple (x: u16) (random: i32) : u16 =
  shrink_u16.shrinker x random
