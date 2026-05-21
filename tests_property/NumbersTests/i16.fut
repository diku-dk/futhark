import "../lib/github.com/diku-dk/cpprandom/random"

--------------------- i16 tests ------------------
-- Uniform i16 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i16 = uniform_int_distribution i16 rng_engine

entry gen_simple (size: i64) (seed: u64) : i16 =
  let rng0 = rng_engine.rng_from_seed [i32.u64 seed]
  --   let (_, x) = rand_i16.rand (i32.lowest, i32.highest) rng0
  let (_, x) = rand_i16.rand (-i16.i64 size, i16.i64 size) rng0
  in x

def simple_succ (x: i16) : i16 =
  i16.abs x

-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

#[prop(gen(gen_simple),shrink(shrink_simple))]
entry prop_simple_succ (x: i16) : bool =
  simple_succ x == i16.abs x

def simple_fail (x: i16) : i16 =
  i16.abs x

-- sometimes fails

#[prop(gen(gen_simple),shrink(shrink_simple))]
entry prop_simple_fail (x: i16) : bool =
  simple_fail x == x

entry shrink_simple (x: i16) (random: u64) : i16 =
  let tactic = random % 2
  in if tactic == 0
     then x // 2
     else if x > 0
     then x - 1
     else x + 1
