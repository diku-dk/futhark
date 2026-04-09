-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/cpprandom/random"

type record = {x: i32, xs: [3]i32}

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 u32 rng_engine

entry gen_simple (size: i64) (seed: i32) : record =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, x) = rand_i32.rand (-100i32, 100i32) rng0
  in {x = x, xs = [x, x+1, x+2]}

let add_to_all (x: i32) (xs: [3]i32) : [3]i32 =
  map (\v -> v + x) xs

let array_eq (a: [3]i32) (b: [3]i32) : bool =
  reduce (&&) true (map2 (\x y -> x == y) a b)

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (r: record) : bool =
  length (add_to_all r.x r.xs) == length r.xs

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (r: record) : bool =
  let ys = add_to_all r.x r.xs
  in array_eq ys r.xs

let step0 (v: i32) : i32 =
  if v > 0 then v - 1
  else if v < 0 then v + 1
  else 0

entry shrink_simple (r: record) (tactic: i32) : (record, i8) =
  if tactic == 0 then
    let x' = step0 r.x
    in ({x = x', xs = r.xs}, i8.bool (x' == r.x))

  else if tactic == 1 then
    let xs' = map step0 r.xs
    in ({x = r.x, xs = xs'}, i8.bool (array_eq xs' r.xs))

  else
    (r, 2)