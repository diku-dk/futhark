-- ==
-- property: prop_simple_succ

-- ==
-- property: prop_simple_fail

import "../lib/github.com/diku-dk/sorts/radix_sort"

let real_sort (xs: []i32) : []i32 =
  radix_sort_int i32.num_bits i32.get_bit xs

-- Bug: after sorting, swap the first two elements (breaks sortedness when they differ).
let flaky_sort (xs: []i32) : []i32 =
  let ys = real_sort xs
  let n = length ys
  in if n < 2 then ys
     else [ys[1]] ++ [ys[0]] ++ drop 2 ys

entry gen_simple_param (m: i64) (with_negatives: bool) (kind: i32) : []i32 =
  let n = i32.i64 m
  let base  = iota m |> map (\i -> i32.i64 i)
  let index = iota m |> map (\i -> i32.i64 i)

  let xs =
    if kind == 0 then
      base
    else if kind == 1 then
      map (\i -> (n-1) - i) base
    else if kind == 2 then
      replicate m 1i32
      |> map2 (\i x -> if i == n-1 then x+1 else x) index
    else
      base

  in if with_negatives && n > 0
     then map2 (\i x -> if i == n-1 then -x else x) index xs
     else xs

entry gen_simple (size: i64) (seed: i32) : []i32 =
  let n = if size < 0 then 0 else size
  let with_negatives = (seed % 2) == 0
  let kind = (i32.abs seed) % 3
  in gen_simple_param n with_negatives kind

let nondecreasing (xs: []i32) : bool =
  let n = length xs
  in if n < 2 then true
     else
       map2 (<=) (take (n-1) xs :> [n-1]i32) (drop 1 xs :> [n-1]i32)
       |> reduce (&&) true

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_fail (input: []i32) : bool =
  nondecreasing (flaky_sort input)

#[prop(gen(gen_simple), shrink(shrink_simple))]
entry prop_simple_succ (input: []i32) : bool =
  nondecreasing (real_sort input)

let shrink_i32_to_witness (v: i32) : i32 =
  if v == 0i32 then 0i32
  else if v > 0i32 then 1i32
  else -1i32

-- status: 0=changed, 1=no-op, 2=stop
entry shrink_simple (xs: []i32) (random: i32) : []i32 =

  let n64 : i64 = length xs
  let n   : i32 = i32.i64 n64
  let tactic : i32 = random % n
  let t   : i32 = if tactic < 0i32 then 0i32 else tactic

  -- Phase 1: shrink one element into {-1,0,1}
  in if t < n then
       if n64 == 0 then
         xs
       else
         let i : i64 = i64.i32 t
         let old = xs[i]
         let new = shrink_i32_to_witness old
         in if new == old then
              xs
            else
              tabulate n64 (\j -> if j == i then new else xs[j])

     -- Phase 2: drop one element
     else if t < 2i32 * n then
       if n64 == 0 then
         xs
       else
         let k : i32 = t - n
         let i : i64 = i64.i32 k
         in if n64 == 1 then
              []
            else
              take i xs ++ drop (i+1) xs

     else
       xs