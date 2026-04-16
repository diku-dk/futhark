import "../libraries/shrinkers/integerShrinker"
import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- u8 tests ------------------
-- Uniform u8 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_u8 = uniform_int_distribution u8 u32 rng_engine
module shrink_u8 = integralShrinker u8

entry gen_simple (size: i64) (seed: i32) : u8 =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, x) = rand_u8.rand (0u8, u8.i64 size) rng0
  in x

let simple_succ (x: u8) : u8 =
  u8.abs x

-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: u8) : bool =
    simple_succ x == u8.abs x

let simple_fail (x: u8) : u8 =
 x-1

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: u8) : bool =
    simple_fail x < x


entry shrink_simple (x: u8) (random: i32) : u8 =
  shrink_u8.shrinker x random
