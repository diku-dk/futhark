-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

-- More complex test for a record with an int inside
import "../lib/github.com/diku-dk/cpprandom/random"
type record = {x: i32, y: i32}
--------------------- record tests ------------------
-- Uniform i32 distribution using minstd_rand (u32 engine) underneath.
module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : record =
    let rng0 = rng_engine.rng_from_seed [seed]
    let (_, x) = rand_i32.rand (-100i32, 100i32) rng0
    in {x = x, y = x-1}

let simple (r: record) : (i32, i32) =
  (i32.abs r.x, i32.abs r.y)

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: record) : bool =
    simple r == (i32.abs r.x, i32.abs r.y)

-- sometimes fails
#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (r: record) : bool =
    simple r == (r.x, r.y)

let step (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (r: record) (random: i32) : record =
  let tactic = random % 2 in
  if tactic == 0 then
    {x = step r.x, y = r.y}
  else
    {x = r.x, y = step r.y} 
