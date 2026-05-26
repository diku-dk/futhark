-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/cpprandom/random"

type record = {x: i32, xs: [3]i32}

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 rng_engine

entry gen_simple (size: i64) (seed: u64) : record =
  let rng0 = rng_engine.rng_from_seed [i32.u64 seed]
  let (_, x) = rand_i32.rand (-i32.i64 size, i32.i64 size) rng0
  in {x = x, xs = [x, x + 1, x + 2]}

def add_to_all (x: i32) (xs: [3]i32) : [3]i32 =
  map (\v -> v + x) xs

def array_eq (a: [3]i32) (b: [3]i32) : bool =
  reduce (&&) true (map2 (\x y -> x == y) a b)

#[prop(gen(gen_simple),shrink(shrink_simple))]
entry prop_simple_succ (r: record) : bool =
  length (add_to_all r.x r.xs) == length r.xs

#[prop(gen(gen_simple),shrink(shrink_simple))]
entry prop_simple_fail (r: record) : bool =
  let ys = add_to_all r.x r.xs
  in array_eq ys r.xs

def step0 (v: i32) : i32 =
  if v > 0
  then v - 1
  else if v < 0
  then v + 1
  else 0

entry shrink_simple (r: record) (random: u64) : record =
  let tactic = random % 2
  in if tactic == 0
     then {x = step0 r.x, xs = r.xs}
     else {x = r.x, xs = map step0 r.xs}
