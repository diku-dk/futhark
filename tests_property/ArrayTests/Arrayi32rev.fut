-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

-- Simple array shrinking demo (new protocol: (x,tactic)->(x',status:i8))
--
-- We test: "all elements are 1".
-- One generator always succeeds, one generator introduces a 0 so it fails.
--
-- Shrinker order (SWAPPED):
--   1) shrink scalars toward 1 (set one element to 1 by index = tactic)
--   2) remove values (drop one element by index = tactic - n)
--
-- status i8:
--   0 = produced candidate (use it; runner restarts tactic from 0 on FAIL)
--   1 = no-op candidate (runner should advance tactic)
--   2 = stop (no more tactics)

type~ arr = []i32

let all_equal_1 (x: i32) : bool =
  x == 1i32

let prop_all_ones (xs: arr) : bool =
  map all_equal_1 xs |> reduce (&&) true

-- Succeeding generator: all 1s
entry gen_record_sums (size: i64) (_: i32) : arr =
  let n = if size < 0 then 0 else size
  in replicate n 1i32

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: arr) : bool =
  prop_all_ones input

-- Failing generator: first element 1, rest 0 (if size>1)
entry gen_record_sums_fail (size: i64) (_: i32) : arr =
  let n = if size < 0 then 0 else size
  let idx = iota n |> map (\i -> i32.i64 i)
  in map (\i -> if i == 0i32 then 1i32 else 0i32) idx

entry shrink_arr (xs: arr) (random: i32) : arr =
  let n64 : i64 = length xs
  in if n64 == 0 then
       xs
     else
       let r0 = i64.i32 random
       let r = if r0 < 0 then -r0 else r0
       let t = r % (2 * n64)

       -- Phase 1: try to set one element to 1.
       in if t < n64 then
            let old = xs[t]
            in if old == 1i32 then
                 -- Avoid returning the same candidate. Fall back to dropping.
                 if n64 == 1 then
                   []
                 else
                   take t xs ++ drop (t + 1) xs
               else
                 tabulate n64 (\i -> if i == t then 1i32 else xs[i])

          -- Phase 2: remove one element.
          else
            let i = t - n64
            in if n64 == 1 then
                 []
               else
                 take i xs ++ drop (i + 1) xs

#[prop(gen(gen_record_sums_fail), shrink(shrink_arr))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_ones input