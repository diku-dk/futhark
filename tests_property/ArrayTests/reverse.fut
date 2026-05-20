import "../libraries/shrinkers/integerShrinker"
import "../lib/github.com/diku-dk/cpprandom/random"

module rnge = minstd_rand
module rand_i32 = uniform_int_distribution i32 rnge
module shrink_i32 = integerlShrinkers i32

entry gen_i32_array (size: i64) (seed: u64) : []i32 =
  tabulate size (\i ->
                   let state = i32.u64 (seed + u64.i64 i)
                   let rng0 = rnge.rng_from_seed [state]
                   let (_, x) = rand_i32.rand (-i32.i64 size, i32.i64 size) rng0 in x)

def reverse_bad (xs: []i32) : []i32 = xs

-- ==
-- property: prop_reverse_involution

-- ==
-- property: prop_reverse_involution_bad

-- ==
-- property: prop_reverse_involution_bad2

#[prop(gen(gen_i32_array))]
entry prop_reverse_involution (xs: []i32) : bool =
  and (map2 (==) (reverse (reverse xs)) xs)

#[prop(gen(gen_i32_array),shrink(shrinker))]
entry prop_reverse_involution_bad (xs: []i32) : bool =
  and (map2 (==) (reverse_bad (reverse xs)) xs)

#[prop(gen(gen_i32_array))]
entry prop_reverse_involution_bad2 (xs: []i32) : bool =
  and (map2 (==) (reverse_bad (reverse xs)) xs)

entry shrinker (xs: []i32) (random: i32) : []i32 =
  let n = length xs
  in if n == 0
     then xs
     else let tactic = random % 2
          let i = (i32.abs random) % i32.i64 n
          in if tactic == 0
             then -- Tactic 0: Shrink a single element at index i
                  let newVal = shrink_i32.shrinker xs[i] random
                  in copy xs with [i] = newVal
             else -- Tactic 1: Remove the element at index i (shrink length)
                  let front = take (i64.i32 i) xs
                  let back = drop (i64.i32 i + 1) xs
                  in concat front back
