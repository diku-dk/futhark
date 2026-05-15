import "../lib/github.com/diku-dk/cpprandom/random"

------------------ function-return property test ------------------

module rng_engine = minstd_rand
module rand_i32 = uniform_int_distribution i32 rng_engine

entry gen_i32 (size: i64) (seed: i32) : i32 =
  let rng0 = rng_engine.rng_from_seed [seed]
  let (_, x) = rand_i32.rand (-i32.i64 size, i32.i64 size) rng0
  in x

entry shrink_i32 (x: i32) (_random: i32) : i32 =
  if x == 0i32
  then x
  else x / 2i32

-- ==
-- property: prop_returns_function

#[prop(gen(gen_i32),shrink(shrink_i32))]
entry prop_returns_function (x: i32) : (i32 -> bool) =
  \y -> x == y
