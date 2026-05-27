-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/cpprandom/random"
type record = {x: {z: i32, b: i32}, y: {o: i32, p: i32}}

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 rng_engine

def inner_eq (r1: {z: i32, b: i32}) (r2: {o: i32, p: i32}) : bool =
  r1.z == r2.o && r1.b == r2.p

entry gen_simple (size: i64) (seed: u64) : record =
  let rng0 = rng_engine.rng_from_seed [i32.u64 seed]
  let (_, v) = rand_i32.rand (- i32.i64 size, i32.i64 size) rng0
  in { x = {z = v, b = v + 1}
     , y = {o = v, p = v + 1}
     }

#[prop(gen(gen_simple),shrink(shrink_simple))]
entry prop_simple_succ (r: record) : bool =
  inner_eq r.x r.y

entry gen_simple_fail (size: i64) (seed: u64) : record =
  let rng0 = rng_engine.rng_from_seed [i32.u64 seed]
  let (_, v) = rand_i32.rand (- i32.i64 size, i32.i64 size) rng0
  in { x = {z = v, b = v + 1}
     , y = {o = v + 1, p = v + 1}
     }

#[prop(gen(gen_simple_fail),shrink(shrink_simple))]
entry prop_simple_fail (r: record) : bool =
  inner_eq r.x r.y

def step0 (v: i32) : i32 =
  if v > 0
  then v - 1
  else if v < 0
  then v + 1
  else 0

entry shrink_simple (r: record) (random: u64) : record =
  let tactic = random % 6
  in if tactic == 0
     then let z' = step0 r.x.z
          let o' = step0 r.y.o
          let r' =
            { x = {z = z', b = r.x.b}
            , y = {o = o', p = r.y.p}
            }
          in r'
     else if tactic == 1
     then let b' = step0 r.x.b
          let p' = step0 r.y.p
          let r' =
            { x = {z = r.x.z, b = b'}
            , y = {o = r.y.o, p = p'}
            }
          in r'
     else if tactic == 2
     then let z' = step0 r.x.z
          let r' = {x = {z = z', b = r.x.b}, y = r.y}
          in r'
     else if tactic == 3
     then let b' = step0 r.x.b
          let r' = {x = {z = r.x.z, b = b'}, y = r.y}
          in r'
     else if tactic == 4
     then let o' = step0 r.y.o
          let r' = {x = r.x, y = {o = o', p = r.y.p}}
          in r'
     else let p' = step0 r.y.p
          let r' = {x = r.x, y = {o = r.y.o, p = p'}}
          in r'
