import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- u16 tests ------------------
-- Uniform u16 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_u16 = uniform_int_distribution u16 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : u16 =
  let rng0 = rng_engine.rng_from_seed [seed]
--   let (_, x) = rand_i16.rand (i32.lowest, i32.highest) rng0
  let (_, x) = rand_u16.rand (0u16, u16.i64 size) rng0
  in x

let simple_succ (x: u16) : u16 =
  u16.abs x

--==
-- property: prop_simple_succ

--==
-- property: prop_simple_fail

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: u16) : bool =
    simple_succ x == u16.abs x

let simple_fail (x: u16) : u16 =
  x-1

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: u16) : bool =
    simple_fail x < x

entry shrink_simple (x: u16) (tactic: i32) : (u16, i8) =
  if tactic == 0 then
    if x > 0 then
      (x - 1, i8.bool (x - 1 == x))
    else
      (x + 1, i8.bool (x + 1 == x)) 
  else 
    (x, 2) 

