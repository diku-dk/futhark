import "../lib/github.com/diku-dk/cpprandom/random"
type pair = ({a: i32, b: i32}, {o: i32, p: i32} )

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

let inner_eq (r1: {a: i32, b: i32}) (r2: {o: i32, p: i32}) : bool =
  r1.a == r2.o && r1.b == r2.p

entry gen_simple (size: i64) (seed: i32) : pair =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in ({a = v,   b = v+1},
      {o = v,   p = v+1})

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: pair) : bool =
  inner_eq r.0 r.1

entry gen_simple_fail (size: i64) (seed: i32) : pair =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, v) = rand_i32.rand (-100i32, 100i32) rng0
  in ({a = v,   b = v+1},
      {o = v+1, p = v+1})

#[prop(gen(gen_simple_fail), shrink(shrink_simple))]
entry prop_simple_fail (r: pair) : bool =
  inner_eq r.0 r.1

let step0 (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (r: pair) (tactic: i32) : (pair, i8) =
  if tactic == 0 then
    let a' = step0 r.0.a
    let o' = step0 r.1.o
    let r' = ({a = a', b = r.0.b},
              {o = o', p = r.1.p})
    in (r', i8.bool (a' == r.0.a && o' == r.1.o))

  else if tactic == 1 then
    let b' = step0 r.0.b
    let p' = step0 r.1.p
    let r' = ({a = r.0.a, b = b'},
              {o = r.1.o, p = p'})
    in (r', i8.bool (b' == r.0.b && p' == r.1.p))

  else if tactic == 2 then
    let a' = step0 r.0.a
    let r' = ({a = a', b = r.0.b}, r.1)
    in (r', i8.bool (a' == r.0.a))

  else if tactic == 3 then
    let b' = step0 r.0.b
    let r' = ({a = r.0.a, b = b'}, r.1)
    in (r', i8.bool (b' == r.0.b))

  else if tactic == 4 then
    let o' = step0 r.1.o
    let r' = (r.0, {o = o', p = r.1.p})
    in (r', i8.bool (o' == r.1.o))

  else if tactic == 5 then
    let p' = step0 r.1.p
    let r' = (r.0, {o = r.1.o, p = p'})
    in (r', i8.bool (p' == r.1.p))

  else
    (r, 2)