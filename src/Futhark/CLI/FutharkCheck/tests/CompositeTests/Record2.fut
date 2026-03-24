-- More complex test for a {x: i32, y: i32} with an int inside
import "../lib/github.com/diku-dk/cpprandom/random"
--------------------- {x: i32, y: i32} tests ------------------
-- Uniform i32 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : {x: i32, y: i32} =
    let rng0 = rng_engine.rng_from_seed [seed]
    let (_, x) = rand_i32.rand (-100i32, 100i32) rng0
    in {x = x, y = x-1}

let simple (r: {x: i32, y: i32}) : (i32, i32) =
  (i32.abs r.x, i32.abs r.y)

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: {x: i32, y: i32}) : bool =
    simple r == (i32.abs r.x, i32.abs r.y)

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (r: {x: i32, y: i32}) : bool =
    simple r == (r.x, r.y)

let step (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (r: {x: i32, y: i32}) (tactic: i32) : ({x: i32, y: i32}, i8) =
  if tactic == 0 then
    let x' = step r.x
    in ({x = step r.x, y = r.y}, i8.bool (x'==r.x))  -- done=true if x can't be changed
  else if tactic == 1 then
    let y' = step r.y
    in ({x = r.x, y = step r.y}, i8.bool (y'==r.y))  -- done=true if y can't be changed
  else
    (r, 2)
