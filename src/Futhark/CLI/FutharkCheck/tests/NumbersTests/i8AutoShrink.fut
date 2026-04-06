import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- i8 tests ------------------
-- Uniform i8 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i8 = uniform_int_distribution i8 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : i8 =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, x) = rand_i8.rand (-i8.i64 size, i8.i64 size) rng0
  in x

let simple_succ (x: i8) : i8 =
  i8.abs x


--==
-- property: prop_simple_succ

--==
-- property: prop_simple_fail

-- suceeds
#[prop(gen(gen_simple), shrink(auto))]
entry prop_simple_succ (x: i8) : bool =
    simple_succ x == i8.abs x

let simple_fail (x: i8) : i8 =
  i8.abs x

-- sometimes fails
#[prop(gen(gen_simple), shrink(auto))]
entry prop_simple_fail (x: i8) : bool =
    simple_fail x == x
