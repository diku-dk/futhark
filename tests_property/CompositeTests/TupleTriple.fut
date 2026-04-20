-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/cpprandom/random"

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

let triple_eq (t: (i32, i32, i32)) : bool =
  t.0 == t.1 && t.1 == t.2

entry gen_simple (size: i64) (seed: i32) : (i32, i32, i32) =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in (v, v, v)

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (t: (i32, i32, i32)) : bool =
  triple_eq t

entry gen_simple_fail (size: i64) (seed: i32) : (i32, i32, i32) =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in (v, v+1, v)

#[prop(gen(gen_simple_fail), shrink(shrink_simple))]
entry prop_simple_fail (t: (i32, i32, i32)) : bool =
  triple_eq t

let step0 (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (t: (i32, i32, i32)) (random: i32) : (i32, i32, i32) =
  let tactic = random % 3 in
  let (a, b, c) = t in
  if tactic == 0 then
    let a' = step0 a
    in (a', b, c)
  else if tactic == 1 then
    let b' = step0 b
    in (a, b', c)
  else 
    let c' = step0 c
    in (a, b, c')