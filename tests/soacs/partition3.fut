def is_sorted [n] (xs: [n]i64) : bool =
  all (\i -> i == 0 || xs[i - 1] < xs[i]) (indices xs)

def is_partition [n] [m] (p: i32 -> bool) (ts: [n]i32) (fs: [m]i32) : bool =
  all p ts && all (not <-< p) fs

-- ==
-- entry: test_partition_stable test_semipartition_semistable
-- random input { [1000]bool }
-- output { true }

-- ==
-- entry: test_partition test_semipartition
-- random input { [1000]i32 }
-- output { true }

entry test_partition [n] (xs: [n]i32) =
  let p = (== 0) <-< (% 2)
  let (ts, fs) = partition p xs
  in is_partition p ts fs

entry test_semipartition [n] (xs: [n]i32) =
  let p = (== 0) <-< (% 2)
  let (ts, fs) = semipartition p xs
  in is_partition p ts fs

entry test_partition_stable [n] (xs: [n]bool) =
  let zs = zip xs (indices xs)
  let (ts, fs) = partition (.0) zs
  in is_sorted (map (.1) ts)
     && is_sorted (map (.1) fs)

entry test_semipartition_semistable [n] (xs: [n]bool) =
  let zs = zip xs (indices xs)
  let (ts, fs) = semipartition (.0) zs
  in is_sorted (map (.1) ts)
     && is_sorted (map (.1) (reverse fs))
