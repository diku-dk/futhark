-- | Work-efficient parallel mergesort
--
-- Author: Sam Westrick <shwestrick@gmail.com>.

------------------------------------------------------------------------------
-- Double binary search:
-- Split sorted sequences s and t into (s1, s2) and (t1, t2) such that the
-- largest items of s1 and t1 are smaller than the smallest items of s2 and t2.
-- The desired output size |s1|+|t1| is given as a parameter.
--
-- Specifically, `split_count leq s t k` returns `(m, n)` where:
--   (s1, s2) = (s[:m], s[m:])
--   (t1, t2) = (t[:n], t[n:])
--   m+n = k
--   max(s1) <= min(t2)
--   max(t1) <= min(s2)
--
-- Note that there are many possible solutions, so we also mandate that `m`
-- should be minimized.
--
-- Work: O(log(|s|+|t|))
-- Span: O(log(|s|+|t|))

local
def split_count 'a (leq: a -> a -> bool) (s: []a) (t: []a) k : (i64, i64) =
  let normalize ((slo, shi), (tlo, thi), count) =
    let slo_orig = slo
    let tlo_orig = tlo
    -- maybe count is small
    let shi = i64.min shi (slo + count)
    let thi = i64.min thi (tlo + count)
    -- maybe count is large
    let slack = (shi - slo) + (thi - tlo) - count
    let slack = i64.min slack (shi - slo)
    let slack = i64.min slack (thi - tlo)
    let slo = i64.max slo (shi - slack)
    let tlo = i64.max tlo (thi - slack)
    let count = count - (slo - slo_orig) - (tlo - tlo_orig)
    in ((slo, shi), (tlo, thi), count)
  let step ((slo, shi), (tlo, thi), count) =
    if shi - slo <= 0
    then ((slo, shi), (tlo + count, thi), 0)
    else if thi - tlo <= 0
    then ((slo + count, shi), (tlo, thi), 0)
    else if count == 1
    then if leq t[tlo] s[slo]
         then ((slo, shi), (tlo + 1, thi), 0)
         else ((slo + 1, shi), (tlo, thi), 0)
    else let m = count / 2
         let n = count - m
         --  |------|x|-------|
         --  ^      ^         ^
         -- slo   slo+m      shi
         --
         --  |------|y|-------|
         --  ^        ^       ^
         -- tlo     tlo+n    thi
         --

         let leq_y_x =
           n == 0
           || slo + m >= shi
           || leq t[tlo + n - 1] s[slo + m]
         in if leq_y_x
            then ((slo, shi), (tlo + n, thi), count - n)
            else ((slo, shi), (tlo, tlo + n), count)
  let ((slo, _), (tlo, _), _) =
    loop (ss, tt, count) = normalize ((0, length s), (0, length t), k)
    while count > 0 do
      normalize (step (ss, tt, count))
  in (slo, tlo)

------------------------------------------------------------------------------
-- `merge_sequential leq s t n` merges sorted sequences `s` and `t` with the
-- simple sequential algorithm, and outputs the first `n` elements.
--
-- (Passing `n` is convenient for making the type checker confident that
-- the output is exactly size `n`.)
--
-- Requires `s` and `t` already sorted by `leq`

local
def merge_sequential 'a (leq: a -> a -> bool) (s: []a) (t: []a) n : *[n]a =
  let dummy = if length s > 0 then head s else head t
  let (_, data) =
    loop (i, data) = (0, replicate n dummy)
    for k < n do
      let j = k - i
      let (i, x) =
        if j == length t || (i < length s && leq s[i] t[j])
        then (i + 1, s[i])
        else (i, t[j])
      in (i, data with [k] = x)
  in data

------------------------------------------------------------------------------
-- `merge_adjacent leq s mid block_size` merges s[:mid] with s[mid:]
-- Requires s[:mid] and s[mid:] both individually already sorted
-- Requires n = mid+mid
-- Requires n divisible by block_size
--
-- High-level idea is:
--   1. Conceptually divide the **output** into blocks.
--   2. Use `split_count` to compute, for each block starting offset i
--      (i.e., where i in {0, block_size, 2*block_size, ...}), use
--      `split_count` to count how many elements from s and t will come
--      before offset i in the output. These counts are called `splitters`
--      in the code.
--   3. Compute the output blocks in parallel. For each block, we sequentially
--      merge one block.
--
-- Technically, for work-efficiency, block_size should be Omega(log n),
-- because `split_count` has approx O(log n) cost, and we do n/block_size
-- calls to `split_count`.
--
-- In other words, we should be careful to pick a block_size that is
-- **not too small**. In practice, however, it's difficult to screw this up,
-- because for feasible values of `n`, we can assume `log n` is effectively
-- constant. block_size = 8 seems to work well.
--
-- Work: O(n)
-- Span: O(block_size + log n)

local
def merge_adjacent 'a [n] (leq: a -> a -> bool) (s: [n]a) mid block_size : *[n]a =
  if n < 10
  then merge_sequential leq s[:mid] s[mid:] n
  else let num_blocks = assert (n % block_size == 0) (n / block_size)
       let splitters =
         tabulate (1 + num_blocks) (\i -> split_count leq s[:mid] s[mid:] (i * block_size))
       let block b: [block_size]a =
         let (slo, tlo) = splitters[b]
         let (shi, thi) = splitters[b + 1]
         in merge_sequential leq s[slo:shi] s[mid + tlo:mid + thi] block_size
       in take n (flatten (tabulate num_blocks block))

------------------------------------------------------------------------------
-- `small_insertion_sort leq s` is faster than merge sort for very small
-- sequences (e.g., |s| <= 20)

local
def small_insertion_sort 't [n] (leq: t -> t -> bool) (s: *[n]t) : *[n]t =
  if n <= 1
  then s
  else let gt x y = !(leq x y)
       in loop s for i in 0...(n - 2) do
            let (s, _) =
              loop (s, j) = (s, i)
              while j >= 0 && gt s[j] s[j + 1] do
                let tmp = copy s[j]
                let s = s with [j] = copy s[j + 1]
                let s = s with [j + 1] = tmp
                in (s, j - 1)
            in s

-----------------------------------------------------------------------------
-- a couple utilities

local
def smallest_pow_2_geq_than k =
  loop (x: i64) = 1 while x < k do 2 * x

local
def greatest_divisor_leq_than upper_bound n =
  -- find smallest d such that d|n and n/d <= upper_bound
  let upper_bound = assert (upper_bound >= 1) upper_bound
  let d = loop (d: i64) = 1 while n / d > upper_bound || n % d != 0 do d + 1
  in n / d

-- | Work-efficient parallel mergesort:
--   `merge_sort_with_params {max_block_size, max_merge_block_size} leq s`
--
-- Any values for {max_block_size, max_merge_block_size} are
-- acceptable, but do heavily impact performance. See
-- `merge_sort`@term below for reasonable choices that seem to work
-- well in practice.
def merge_sort_with_params [n] 't {max_block_size, max_merge_block_size} (leq: t -> t -> bool) (s: [n]t) : [n]t =
  -- High-level idea is:
  --   1. Divide the input into many small blocks (of size at most max_block_size)
  --   2. Sort all the small blocks in parallel (using small_insertion_sort)
  --   3. Merge the blocks upwards (classic loopification of mergesort)
  --
  -- The tricky thing is that, to make the last step most efficient, we want a
  -- exactly a power-of-two number of blocks.
  --
  -- We could of course just pad everything to the nearest power-of-two size, but
  -- this risks incurring a ~2x work overhead in the worst case, which probably
  -- will result in 2x slowdown in practice for large sequences
  --
  -- So instead, we try to maximize block_size and minimize num_blocks subject to
  -- the following inequalities, which guarantees that we need approximately at
  -- most n/max_block_size padding in the worst case.
  --   block_size <= max_block_size
  --   block_size * 2^num_blocks >= n
  --
  -- Finally, a small technical issue is that, when we merge, we also need to
  -- select a block size for merge to use internally. We call this
  -- `merge_block_size`, and similarly put a bound on it with
  -- `max_merge_block_size`. We need `merge_block_size` to be a divisor of
  -- every 2^i * block_size for i >= 1. So, we pick the greatest divisor of
  -- 2*block_size that satisfies merge_block_size <= max_merge_block_size.
  if length s <= 1
  then s
  else let max_block_size = i64.max 1 max_block_size
       let max_merge_block_size = i64.max 1 max_merge_block_size
       let min_num_blocks = 1 + (n - 1) / max_block_size
       let num_blocks = smallest_pow_2_geq_than min_num_blocks
       let block_size = 1 + (n - 1) / num_blocks
       let padded_n = block_size * num_blocks
       let merge_block_size =
         greatest_divisor_leq_than max_merge_block_size (2 * block_size)
       let max_elem = reduce (\a b -> if leq a b then b else a) s[0] s
       let sorted_blocks =
         flatten (tabulate num_blocks (\i ->
                                         let block =
                                           tabulate block_size (\j ->
                                                                  let k = i * block_size + j
                                                                  in if k < n then s[k] else max_elem)
                                         in small_insertion_sort leq block))
       let (data, _) =
         loop (data, stride) = (sorted_blocks, block_size)
         while stride < padded_n do
           let next_stride = 2 * stride
           let num_merges = padded_n / next_stride
           let data =
             flatten (tabulate num_merges (\mi ->
                                             let start = mi * next_stride
                                             let piece = take next_stride (drop start data)
                                             in merge_adjacent leq piece stride merge_block_size))
           in (data, next_stride)
       in take n data

-----------------------------------------------------------------------------

-- | Implements `merge_sort leq s` with reasonable default parameters. Here
-- `leq` is a less-than-or-equal comparison function on elements of type `t`,
-- and `s` is an array of `n` elements of type `t`. The default parameters are
-- max_block_size = 20 and max_merge_block_size = 8.
--
-- Work: **O(n log n)**
--
-- Span: **O(log**2 n)**
def merge_sort [n] 't (leq: t -> t -> bool) (s: [n]t) : [n]t =
  -- Note: this selection of parameters seems to work well in practice, but
  -- could be investigated more carefully.
  merge_sort_with_params {max_block_size = 20, max_merge_block_size = 8} leq s

def merge_sort_by_key [n] 't 'k (key: t -> k) (leq: k -> k -> bool) (s: [n]t) : [n]t =
  zip (map key s) (iota n)
  |> merge_sort (\(x, _) (y, _) -> leq x y)
  |> map (\(_, i) -> s[i])

def merge_sort_with_params_by_key [n] 't 'k params (key: t -> k) (leq: k -> k -> bool) (s: [n]t) : [n]t =
  zip (map key s) (iota n)
  |> merge_sort_with_params params (\(x, _) (y, _) -> leq x y)
  |> map (\(_, i) -> s[i])
