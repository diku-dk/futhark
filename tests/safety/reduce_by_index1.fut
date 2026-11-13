-- ==
-- compiled random input { [20000]f32 } error: l != 1337

def main [n] (xs: *[n]f32) =
  let xs[1337] = f32.lowest
  let op i j =
    let l = if xs[i] < xs[j] then i else j
    in assert (l != 1337) l
  in hist op 0 3 (map (% 3) (iota n)) (iota n)

-- An operator on pairs is applied while holding a lock.  With a limit of 2, the
-- second update of a bucket fails, while other threads may be waiting for the
-- lock.  With many buckets, the global-memory version is used, whose locks
-- persist across kernel launches; with one bucket, the shared-memory version is
-- used, where all threads of a block wait for the same lock.  The datasets are
-- run in order by the same process, so the later ones check that the failure
-- did not leave a lock held.

def hist_pairs (k: i64) (h: i64) n =
  let op (a: i64, b: i64) (c: i64, d: i64) = assert (a + c < k) (a + c, b + d)
  let (as, bs) =
    unzip (hist op (0, 0) h (replicate n 0) (map (\i -> (1, i)) (iota n)))
  in (as[0], bs[0])

-- ==
-- entry: locking_fail
-- input { 10000i64 2000i64 } error: a \+ c < k
-- input { 1i64 2000i64 } error: a \+ c < k

entry locking_fail h n = hist_pairs 2 h n

-- ==
-- entry: locking_ok
-- input { 10000i64 50i64 } output { 50i64 1225i64 }
-- input { 1i64 50i64 } output { 50i64 1225i64 }

entry locking_ok h n = hist_pairs 1000000 h n

-- The operator cannot fail but needs a lock, while the computation of the
-- buckets fails.  The failure happens before the lock is acquired, so it must
-- not lead to the code that releases it.  All buckets are out of bounds and
-- thus ignored, so a thread that releases the lock for a bucket it computed
-- for a previous element accesses the locks out of bounds.

-- ==
-- entry: map_fail
-- input { 10000i64 10000i64 } error: i != n - 1
-- input { 1i64 10000i64 } error: i != n - 1

entry map_fail h n =
  let op (a: i64, i: i64) (b: i64, j: i64) =
    if a > b then (a, i) else if b > a then (b, j) else (a, i64.min i j)
  let is = map (\i -> assert (i != n - 1) (-1 - i)) (iota n)
  let (as, is) = unzip (hist op (i64.lowest, -1) h is (map (\i -> (i, i)) (iota n)))
  in (as[0], is[0])

-- As above, but the operator may also fail, although it never does here.

-- ==
-- entry: map_op_fail
-- input { 10000i64 10000i64 } error: i != n - 1
-- input { 1i64 10000i64 } error: i != n - 1

entry map_op_fail h n =
  let op (a: i64, b: i64) (c: i64, d: i64) = assert (a + c <= n) (a + c, b + d)
  let is = map (\i -> assert (i != n - 1) (-1 - i)) (iota n)
  let (as, bs) = unzip (hist op (0, 0) h is (map (\i -> (1, i)) (iota n)))
  in (as[0], bs[0])
