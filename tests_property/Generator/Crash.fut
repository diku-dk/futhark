import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- i8 tests ------------------
-- Uniform i8 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i8 = uniform_int_distribution i8 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : i8 =
  let rng0 = rng_engine.rng_from_seed [seed]
  -- let (_, x) = rand_i8.rand (-100i8, 100i8) rng0
  let (_, x) = rand_i8.rand (-i8.i64 size, i8.i64 size) rng0
  in x/0

entry gen_simple2 (size: i64) (seed: i32) : i8 =
  let rng0 = rng_engine.rng_from_seed [seed]
  -- let (_, x) = rand_i8.rand (-100i8, 100i8) rng0
  let (_, x) = rand_i8.rand (-i8.i64 size, i8.i64 size) rng0
  in x

entry gen_simple3 (size: i64) (seed: i32) : i8 =
  let rng0 = rng_engine.rng_from_seed [seed]
  -- let (_, x) = rand_i8.rand (-100i8, 100i8) rng0
  let (_, x) = rand_i8.rand (-i8.i64 size, i8.i64 size) rng0
  in x

let simple_succ (x: i8) : i8 =
  i8.abs x

-- ==
-- property: prop_simple_succ2
#[prop(gen(gen_simple2), shrink(shrink_simple))]
entry prop_simple_succ2 (x: i8) : bool =
    simple_succ x == i8.abs x

-- ==
-- property: prop_simple_succ
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: i8) : bool =
    simple_succ x == i8.abs x

-- ==
-- property: prop_simple_succ3
#[prop(gen(gen_simple3), shrink(shrink_simple))]
entry prop_simple_succ3 (x: i8) : bool =
    simple_succ x == i8.abs x

entry shrink_simple (x: i8) (_random: i32) : i8 =
  if x > 0 then
    x - 2
  else
    x + 2
