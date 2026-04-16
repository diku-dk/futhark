-- | Bitonic sort.
--
-- Runs in *O(n log²(n))* work and *O(log²(n))* span.  Internally pads
-- the array to the next power of two, so a poor fit for some array
-- sizes.
--
-- ## See Also
--
-- * `merge_sort`@term@"merge_sort"

local
def log2 (n: i64) : i64 =
  let r = 0
  let (r, _) =
    loop (r, n) while 1 < n do
      let n = n / 2
      let r = r + 1
      in (r, n)
  in r

local
def ensure_pow_2 [n] 't ((<=): t -> t -> bool) (xs: [n]t) : (*[]t, i64) =
  if n == 0
  then (copy xs, 0)
  else let d = log2 n
       in if n == 2 ** d
          then (copy xs, d)
          else let largest = reduce (\x y -> if x <= y then y else x) xs[0] xs
               in ( concat xs (replicate (2 ** (d + 1) - n) largest)
                  , d + 1
                  )

local
def kernel_par [n] 't ((<=): t -> t -> bool) (a: *[n]t) (p: i64) (q: i64) : *[n]t =
  let d = 1 << (p - q)
  in tabulate n (\i ->
                   let a_i = a[i]
                   let up1 = ((i >> p) & 2) == 0
                   in if (i & d) == 0
                      then let a_iord = a[i | d]
                           in if a_iord <= a_i == up1
                              then a_iord
                              else a_i
                      else let a_ixord = a[i ^ d]
                           in if a_i <= a_ixord == up1
                              then a_ixord
                              else a_i)

-- | Sort an array in increasing order.
def bitonic_sort [n] 't ((<=): t -> t -> bool) (xs: [n]t) : *[n]t =
  -- We need to pad the array so that its size is a power of 2.  We do
  -- this by first finding the largest element in the input, and then
  -- using that for the padding.  Then we know that the padding will
  -- all be at the end, so we can easily cut it off.
  let (xs, d) = ensure_pow_2 (<=) xs
  in (loop xs for i < d do
        loop xs for j < i + 1 do kernel_par (<=) xs i j)[:n]

-- | Like `bitonic_sort`, but sort based on key function.
def bitonic_sort_by_key [n] 't 'k (key: t -> k) ((<=): k -> k -> bool) (xs: [n]t) : [n]t =
  zip (map key xs) (iota n)
  |> bitonic_sort (\(x, _) (y, _) -> x <= y)
  |> map (\(_, i) -> xs[i])
