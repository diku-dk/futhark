-- Simple array shrinking demo (new protocol: (x,tactic)->(x',status:i8))
--
-- We test: "all elements are 1".
-- One generator always succeeds, one generator introduces a 0 so it fails.
--
-- Shrinker order:
--   1) remove values (drop one element by index = tactic)
--   2) shrink scalars toward 1 (set one element to 1 by index = tactic - n)
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

#[prop(gen(gen_record_sums_fail), shrink(auto))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_ones input
