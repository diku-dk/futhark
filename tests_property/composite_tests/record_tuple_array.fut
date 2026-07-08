-- ==
-- property: prop_simple_succ
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/cpprandom/random"

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 rng_engine

type inner = {xs: [4]i32, t: (i32, i32)}

type rec =
  { a: inner
  , b: (inner, [4]i32)
  , c: [4]inner
  }

def step0 (v: i32) : i32 =
  if v > 0 then v - 1 else if v < 0 then v + 1 else v

def mk (ok: bool) (v: rec) : []rec =
  map (\t -> t.1) (filter (\t -> t.0) [(ok, v)])

entry gen_simple (size: i64) (seed: u64) : rec =
  let rng0 = rng_engine.rng_from_seed [i32.u64 seed]
  let (rng1, a0) = rand_i32.rand (-100i32, 100i32) rng0
  let (rng2, a1) = rand_i32.rand (-100i32, 100i32) rng1
  let (rng3, a2) = rand_i32.rand (-100i32, 100i32) rng2
  let (rng4, a3) = rand_i32.rand (-100i32, 100i32) rng3
  let (rng5, b0) = rand_i32.rand (-100i32, 100i32) rng4
  let (rng6, b1) = rand_i32.rand (-100i32, 100i32) rng5
  let (rng7, b2) = rand_i32.rand (-100i32, 100i32) rng6
  let (_, b3) = rand_i32.rand (-100i32, 100i32) rng7
  let a_xs: [4]i32 = [a0, a1, a2, a3]
  let b_xs: [4]i32 = [b0, b1, b2, b3]
  let a_inner: inner = {xs = a_xs, t = (a0, a1)}
  let b_inner: inner = {xs = b_xs, t = (b0, b1)}
  let c_arr: [4]inner =
    [ {xs = [a0, a1, a2, a3], t = (a2, a3)}
    , {xs = [a1, a2, a3, a0], t = (a1, a2)}
    , {xs = [b0, b1, b2, b3], t = (b2, b3)}
    , {xs = [b1, b2, b3, b0], t = (b1, b2)}
    ]
  in { a = a_inner
     , b = (b_inner, [a3, a2, a1, a0])
     , c = c_arr
     }

def simple_succ (r: rec) : i32 =
  i32.abs r.a.xs[0] + i32.abs r.b.0.t.0 + i32.abs r.c[0].xs[0]

#[prop(gen(gen_simple))]
entry prop_simple_succ (r: rec) : bool =
  simple_succ r
  == (i32.abs r.a.xs[0] + i32.abs r.b.0.t.0 + i32.abs r.c[0].xs[0])


entry gen_simple_fail (size: i64) (seed: u64) : rec =
  gen_simple size seed

def simple_fail (r: rec) : i32 =
  i32.abs r.a.xs[0] + i32.abs r.b.0.t.0 + i32.abs r.c[0].xs[0] + 1

#[prop(gen(gen_simple_fail))]
entry prop_simple_fail (r: rec) : bool =
  simple_fail r
  == (i32.abs r.a.xs[0] + i32.abs r.b.0.t.0 + i32.abs r.c[0].xs[0])
