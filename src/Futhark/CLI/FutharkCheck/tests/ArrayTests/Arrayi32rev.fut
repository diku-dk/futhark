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

-- New-protocol shrinker for []i32 (SWAPPED order)
entry shrink_arr (xs: arr) (tactic: i32) : (arr, i8) =
  let s0 : i8 = i8.i32 0
  let s1 : i8 = i8.i32 1
  let s2 : i8 = i8.i32 2

  let n64 : i64 = length xs
  let n  : i32 = i32.i64 n64
  let t  : i32 = if tactic < 0i32 then 0i32 else tactic

  -- ----- Phase 1: shrink scalars toward 1 (t in [0..n-1]) -----
  in if t < n then
       if n64 == 0 then
         (xs, s2)
       else
         let ki : i64 = i64.i32 t
         let old = xs[ki]
         in if old == 1i32 then
              (xs, s1)   -- no-op; advance tactic
            else
              (tabulate n64 (\i -> if i == ki then 1i32 else xs[i]), s0)

     -- ----- Phase 2: remove one element (t in [n..2n-1]) -----
     else if t < 2i32 * n then
       if n64 <= 0 then
         (xs, s2)
       else
         let k : i32 = t - n
         let i : i64 = i64.i32 k in
         if n64 == 1 then
           ([], s0)
         else
           let pre  = take i xs
           let post = drop (i+1) xs
           in (pre ++ post, s0)

     else
       (xs, s2)

#[prop(gen(gen_record_sums_fail), shrink(shrink_arr))]
entry prop_record_sums_fail (input: arr) : bool =
  prop_all_ones input