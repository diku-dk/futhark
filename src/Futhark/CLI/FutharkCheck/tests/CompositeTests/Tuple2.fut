-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

-- More complex test for a (i32, i32)
import "../lib/github.com/diku-dk/cpprandom/random"

--------------------- (i32, i32) tests ------------------
-- Uniform i32 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : (i32, i32) =
    let rng0 = rng_engine.rng_from_seed [seed]
    let (_, x) = rand_i32.rand (-100i32, 100i32) rng0
    in (x, x-1)

let simple (r: (i32, i32)) : (i32, i32) =
  (i32.abs r.0, i32.abs r.1)

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: (i32, i32)) : bool =
    simple r == (i32.abs r.0, i32.abs r.1)

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (r: (i32, i32)) : bool =
    simple r == (r.0, r.1)

let step (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (r: (i32, i32)) (tactic: i32) : ((i32, i32), i8) =
  if tactic == 0 then
    let x' = step r.0
    in ({0 = step r.0, 1 = r.1}, i8.bool (x'==r.0))  -- done=true if x can't be changed
  else if tactic == 1 then
    let y' = step r.1
    in ({0 = r.0, 1 = step r.1}, i8.bool (y'==r.1))  -- done=true if y can't be changed
  else
    (r, 2)
