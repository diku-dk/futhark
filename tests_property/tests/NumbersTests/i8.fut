import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- i8 tests ------------------
-- Uniform i8 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i8 = uniform_int_distribution i8 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : i8 =
  let rng0 = rng_engine.rng_from_seed [seed]
  -- let (_, x) = rand_i8.rand (-100i8, 100i8) rng0
  let (_, x) = rand_i8.rand (-i8.i64 size, i8.i64 size) rng0
  in x

let simple_succ (x: i8) : i8 =
  i8.abs x


-- Regular test case for the shrinking to fail on.
-- == 
-- entry: prop_simple_succ
-- input {-10i8}
-- output {true}
-- input {10i8}
-- output {true}

-- ==
-- property: prop_simple_succ
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: i8) : bool =
    simple_succ x == i8.abs x

let simple_fail (x: i8) : i8 =
  i8.abs x

-- Regular test case for the shrinking to fail on.
-- == 
-- entry: prop_simple_fail
-- input {10i8}
-- output {true}
-- input {-10i8}
-- output {true}

-- this should not run but it should use num to increase number of tests
-- n==
-- property: prop_simple_fai

-- this should run and all 3 should fail
-- ==
-- property: prop_simple_fail

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: i8) : bool =
    simple_fail x == x

entry shrink_simple (x: i8) (tactic: i32) : (i8, i8) =
  if tactic == 0 then
    if x > 0 then
      (x - 1, i8.bool (x - 1 == x))
    else
      (x + 1, i8.bool (x + 1 == x)) 
  else 
    (x, 2) 

