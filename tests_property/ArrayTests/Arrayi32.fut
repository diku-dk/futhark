-- ==
-- property: prop_record_sums_succ

-- ==
-- property: prop_record_sums_fail

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

def all_equal_1 (x: i32) : bool =
  x == 1

def prop_all_ones (xs: arr) : bool =
  map all_equal_1 xs |> reduce (&&) true

-- Succeeding generator: all 1s
entry gen_record_sums (size: i64) (_: u64) : arr =
  let n = if size < 0 then 0 else size
  in replicate n 1

#[prop(gen(gen_record_sums))]
entry prop_record_sums_succ (input: arr) : bool =
  prop_all_ones input

-- Failing generator: first element 1, rest 0 (if size>1)
entry gen_record_sums_fail (size: i64) (_: u64) : arr =
  let n = if size < 0 then 0 else size
  let idx = iota n |> map (\i -> i32.i64 i)
  in map (\i -> if i == 0 then 1 else 0) idx

-- New-protocol shrinker for []i32
entry shrink_arr (xs: arr) (random: u64) : arr =
  let tactic = random % 4
  let n = length xs
  let t = if tactic < 0 then 0 else tactic
  -- ----- Phase 1: remove one element (t in [0..n-1]) -----
  in if i64.u64 t < n
     then if n <= 0
          then xs
          else if n == 1
          then -- removing would make it empty; allow it (often helps)
               []
          else let i= i64.u64 t
               let pre = take i xs
               let post = drop (i + 1) xs
               in pre ++ post
     else -- ----- Phase 2: shrink scalars toward 1 (t in [n..2n-1]) -----
     if i64.u64 t < 2 * n
     then if n == 0
          then xs
          else let ki = i64.u64 t - n
               -- set xs[k] to 1 if it isn't already
               let old = xs[ki]
               in if old == 1
                  then xs
                  else tabulate n (\i -> if i == ki then 1 else xs[i])
     else xs

#[prop(gen(gen_record_sums_fail),shrink(shrink_arr))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_ones input
