-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/cpprandom/random"

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

let inner_eq (r1: (i32, i32)) (r2: (i32, i32)) : bool =
  r1.0 == r2.0 && r1.1 == r2.1

entry gen_simple (size: i64) (seed: i32) : {x: (i32, i32), y: (i32, i32)} =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in { x = (v, v+1),
       y = (v, v+1) }

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: {x: (i32, i32), y: (i32, i32)}) : bool =
  inner_eq r.x r.y

entry gen_simple_fail (size: i64) (seed: i32) : {x: (i32, i32), y: (i32, i32)} =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in { x = (v, v+1),
       y = (v+1, v+1) }

#[prop(gen(gen_simple_fail), shrink(shrink_simple))]
entry prop_simple_fail (r: {x: (i32, i32), y: (i32, i32)}) : bool =
  inner_eq r.x r.y

let step0 (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple
  (r: {x: (i32, i32), y: (i32, i32)})
  (random: i32)
  : {x: (i32, i32), y: (i32, i32)} =
  let tactic = random % 6 in
  if tactic == 0 then
    let x0' = step0 r.x.0
    let y0' = step0 r.y.0
    let r' = {x = (x0', r.x.1),
              y = (y0', r.y.1)}
    in r'

  else if tactic == 1 then
    let x1' = step0 r.x.1
    let y1' = step0 r.y.1
    let r' = {x = (r.x.0, x1'),
              y = (r.y.0, y1')}
    in r'

  else if tactic == 2 then
    let x0' = step0 r.x.0
    let r' = {x = (x0', r.x.1), y = r.y}
    in r'

  else if tactic == 3 then
    let x1' = step0 r.x.1
    let r' = {x = (r.x.0, x1'), y = r.y}
    in r'

  else if tactic == 4 then
    let y0' = step0 r.y.0
    let r' = {x = r.x, y = (y0', r.y.1)}
    in r'

  else
    let y1' = step0 r.y.1
    let r' = {x = r.x, y = (r.y.0, y1')}
    in r'