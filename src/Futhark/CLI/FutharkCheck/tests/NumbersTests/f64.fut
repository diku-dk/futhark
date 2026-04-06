import "../lib/github.com/diku-dk/cpprandom/random"

------------------ f64 tests ------------------
-- Uniform f64 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand = uniform_real_distribution f64 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : f64 =
  let rng0 = rng_engine.rng_from_seed [seed]
--   let (_, x) = rand_i32.rand (i32.lowest, i32.highest) rng0
  let (_, x) = rand.rand (-f64.i64 size, f64.i64 size) rng0
  in x

let simple_succ (x: f64) : f64 =
  f64.abs x

--==
-- property: prop_simple_succ
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (x: f64) : bool =
    simple_succ x == f64.abs x


let simple_fail (x: f64) : f64 =
  f64.abs x

--==
-- property: prop_simple_fail
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (x: f64) : bool =
    simple_fail x == x

entry shrink_simple (x: f64) (tactic: i32) : (f64, i8) =
  if tactic == 0 then
    if x > 0 then
      (x - 1, i8.bool (x - 1 == x))
    else
      (x + 1, i8.bool (x + 1 == x)) 
  else 
    (x, 2) 

