-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/cpprandom/random"

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

let inner_eq (r1: (i32, i32)) (r2: (i32, i32)) : bool =
  r1.0 == r2.0 && r1.1 == r2.1

entry gen_simple (size: i64) (seed: i32) : ((i32, i32), (i32, i32)) =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in ((v, v+1), (v, v+1))

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: ((i32, i32), (i32, i32))) : bool =
  inner_eq r.0 r.1

entry gen_simple_fail (size: i64) (seed: i32) : ((i32, i32), (i32, i32)) =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in ((v, v+1), (v+1, v+1))

#[prop(gen(gen_simple_fail), shrink(shrink_simple))]
entry prop_simple_fail (r: ((i32, i32), (i32, i32))) : bool =
  inner_eq r.0 r.1

let step0 (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple ((a: i32, b: i32), (c: i32, d: i32)) (tactic: i32)
  : (((i32, i32), (i32, i32)), i8) =
  if tactic == 0 then
    let x0' = step0 a
    let y0' = step0 c
    let r' = ((x0', b), (y0', d))
    in (r', i8.bool (x0' == a && y0' == c))

  else if tactic == 1 then
    let x1' = step0 b
    let y1' = step0 d
    let r' = ((a, x1'), (c, y1'))
    in (r', i8.bool (x1' == b && y1' == d))

  else if tactic == 2 then
    let x0' = step0 a
    let r' = ((x0', b), (c, d))
    in (r', i8.bool (x0' == a))

  else if tactic == 3 then
    let x1' = step0 b
    let r' = ((a, x1'), (c, d))
    in (r', i8.bool (x1' == b))

  else if tactic == 4 then
    let y0' = step0 c
    let r' = ((a, b), (y0', d))
    in (r', i8.bool (y0' == c))

  else if tactic == 5 then
    let y1' = step0 d
    let r' = ((a, b), (c, y1'))
    in (r', i8.bool (y1' == d))

  else
    (((a, b), (c, d)), 2)