import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- i32 tests ------------------
-- Uniform i32 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : i32 =
  let rng0 = rng_engine.rng_from_seed [seed]
  -- let (_, x) = rand_i32.rand (i32.lowest, i32.highest) rng0
  let (_, x) = rand_i32.rand (-i32.i64 size, i32.i64 size) rng0
  in x

let simple_succ (x: i32) : i32 =
  i32.abs x

--==
-- property: prop_simple_succ

--==
-- property: prop_simple_fail

#[prop(gen(gen_simple))]
entry prop_simple_succ (x: i32) : bool =
    simple_succ x == i32.abs x

let simple_fail (x: i32) : i32 =
  i32.abs x

-- sometimes fails
#[prop(gen(gen_simple))]
entry prop_simple_fail (x: i32) : bool =
    simple_fail x == x
