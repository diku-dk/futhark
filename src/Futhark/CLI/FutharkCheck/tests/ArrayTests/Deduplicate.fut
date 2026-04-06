-- ==
-- property: prop_deduplicate_succ

-- ==
-- property: prop_deduplicate_fail

-- Deduplicate demo with NEW shrink protocol: (x,tactic)->(x',status:i8)
--
-- Property we test:
--   after "deduplicate_*", there are no adjacent equal elements.
--
-- Buggy version:
--   deduplicate_fail does adjacent-dedup, then decrements the last element.
--   Minimal counterexample is [0,1] because dedup=[0,1] then last-- => [0,0].

-- ---------- helpers ----------

let no_adjacent_equal (xs: []i32) : bool =
  let n = length xs
  in if n < 2 then true
     else
       map2 (!=) (take (n-1) xs :> [n-1]i32) (drop 1 xs :> [n-1]i32)
       |> reduce (&&) true

let deduplicate_succ (xs: []i32) : []i32 =
  let n = length xs
  in if n < 2 then xs
     else
       let (_, result) =
         loop (i, acc) = (1i32, [xs[0]]) while (i < i32.i64 n) do
           let curr = xs[i]
           let last = acc[length acc - 1]
           in if curr == last then
                (i+1i32, acc)
              else
                (i+1i32, acc ++ [curr])
       in result

let deduplicate_fail (xs: []i32) : []i32 =
  let result = deduplicate_succ xs
  let n = length result
  in if n < 2 then result
     else
       -- break it: decrement the last element
       tabulate n (\i ->
         let x = result[i]
         in if i == n-1 then x - 1i32 else x)

-- ---------- generators ----------

-- Passing generator: already “safe” for the postcondition after succ dedup.
-- (Doesn't matter much; just keeps the demo complete.)
entry gen_deduplicate_ok (size: i64) (_seed: i32) : []i32 =
  let n = if size < 0 then 0 else size
  in tabulate n (\i -> i32.i64 i)   -- strictly increasing

#[prop(gen(gen_deduplicate_ok))]
entry prop_deduplicate_succ (input: []i32) : bool =
  no_adjacent_equal (deduplicate_succ input)

-- Failing generator: ALWAYS returns [0,1] when size>=2, else [0].
-- So prop_deduplicate_fail will always fail when size>=2.
entry gen_deduplicate_bad (size: i64) (_seed: i32) : []i32 =
  let n = if size < 0 then 0 else size
  in if n < 2 then [0i32]
     else [0i32, 1i32]

-- ---------- shrinker (NEW protocol) ----------
-- status i8:
--   0 = changed candidate (use it; runner restarts tactic=0 on FAIL)
--   1 = no-op for this tactic (runner should advance tactic)
--   2 = stop (no more tactics)
--
-- Tactic schedule (for current xs of length n):
--   Phase A (0 .. n-1): set xs[t] to a small canonical value:
--       target(0)=0, target(i>0)=1
--   Phase B (n .. 2n-1): remove element (t-n)
--
-- This tends to shrink toward the minimal witness [0,1].

entry shrink_deduplicate (xs: []i32) (tactic: i32) : ([]i32, i8) =
  let s0 : i8 = i8.i32 0
  let s1 : i8 = i8.i32 1
  let s2 : i8 = i8.i32 2

  let n64 : i64 = length xs
  let n   : i32 = i32.i64 n64
  let t   : i32 = if tactic < 0i32 then 0i32 else tactic

  -- ---- Phase A: shrink scalars toward targets ----
  in if t < n then
       if n64 == 0 then
         (xs, s2)
       else
         let i : i64 = i64.i32 t
         let old = xs[i]
         let target = if t == 0i32 then 0i32 else 1i32
         in if old == target then
              (xs, s1)
            else
              (tabulate n64 (\j -> if j == i then target else xs[j]), s0)

     -- ---- Phase B: remove one element ----
     else if t < 2i32 * n then
       if n64 == 0 then
         (xs, s2)
       else
         let k : i32 = t - n
         let i : i64 = i64.i32 k
         in if n64 == 1 then
              ([], s0)
            else
              (take i xs ++ drop (i+1) xs, s0)

     else
       (xs, s2)

-- ---------- failing property using shrinker ----------

#[prop(gen(gen_deduplicate_bad), shrink(shrink_deduplicate))]
entry prop_deduplicate_fail (input: []i32) : bool =
  no_adjacent_equal (deduplicate_fail input)