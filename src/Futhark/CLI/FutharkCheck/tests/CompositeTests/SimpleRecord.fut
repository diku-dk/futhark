-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

-- Simple tests for a record with an int inside
import "../lib/github.com/diku-dk/cpprandom/random"
type record = {x: i32}
--------------------- record tests ------------------
-- Uniform i32 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : record =
    let rng0 = rng_engine.rng_from_seed [seed]
    let (_, x) = rand_i32.rand (-100i32, 100i32) rng0
    in {x = x}

let simple_succ (r: record) : i32 =
  i32.abs r.x

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: record) : bool =
    simple_succ r == i32.abs r.x

let simple_fail (r: record) : i32 =
  i32.abs r.x
-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (r: record) : bool =
    simple_fail r ==  r.x

let step (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (r: record) (tactic: i32) : (record, i8) =
  if tactic == 0 then
    let x' = step r.x
    in ({x = x'}, i8.bool (x'==r.x))
  else 
    (r, 2)