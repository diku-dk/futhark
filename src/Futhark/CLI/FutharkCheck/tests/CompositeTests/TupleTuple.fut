import "../lib/github.com/diku-dk/cpprandom/random"

type tupleLeft  = (i32, i32)
type tupleRight = (i32, i32)
type tuple = (tupleLeft, tupleRight)

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

let inner_eq (r1: tupleLeft) (r2: tupleRight) : bool =
  r1.0 == r2.0 && r1.1 == r2.1

entry gen_simple (size: i64) (seed: i32) : tuple =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in ((v, v+1), (v, v+1))

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: tuple) : bool =
  inner_eq r.0 r.1

entry gen_simple_fail (size: i64) (seed: i32) : tuple =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in ((v, v+1), (v+1, v+1))

#[prop(gen(gen_simple_fail), shrink(shrink_simple))]
entry prop_simple_fail (r: tuple) : bool =
  inner_eq r.0 r.1

let step0 (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (r: tuple) (tactic: i32) : (tuple, i8) =
  if tactic == 0 then
    let x0' = step0 r.0.0
    let y0' = step0 r.1.0
    let r' = ((x0', r.0.1), (y0', r.1.1))
    in (r', i8.bool (x0' == r.0.0 && y0' == r.1.0))

  else if tactic == 1 then
    let x1' = step0 r.0.1
    let y1' = step0 r.1.1
    let r' = ((r.0.0, x1'), (r.1.0, y1'))
    in (r', i8.bool (x1' == r.0.1 && y1' == r.1.1))

  else if tactic == 2 then
    let x0' = step0 r.0.0
    let r' = ((x0', r.0.1), r.1)
    in (r', i8.bool (x0' == r.0.0))

  else if tactic == 3 then
    let x1' = step0 r.0.1
    let r' = ((r.0.0, x1'), r.1)
    in (r', i8.bool (x1' == r.0.1))

  else if tactic == 4 then
    let y0' = step0 r.1.0
    let r' = (r.0, (y0', r.1.1))
    in (r', i8.bool (y0' == r.1.0))

  else if tactic == 5 then
    let y1' = step0 r.1.1
    let r' = (r.0, (r.1.0, y1'))
    in (r', i8.bool (y1' == r.1.1))

  else
    (r, 2)