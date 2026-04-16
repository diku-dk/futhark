-- | ignore

import "shuffle"

module shuffle = mk_shuffle u32 pcg32

-- ==
-- entry: test_shuffle
-- input { 10i64 } output { 45i64 true }
-- input { 1000i64 } output { 499500i64 true }
entry test_shuffle (n: i64) =
  let rng = pcg32.rng_from_seed [123, i32.i64 n, 42]
  let (_, xs) = shuffle.shuffle rng (iota n)
  let sorted = map2 (<=) xs[:n - 1] (rotate 1 xs)[:n - 1] |> and
  in (i64.sum xs, !sorted)
