-- | Bitonic merge sort.  Runs in *O(n log(n))* work and *O(log(n))*
-- span.  Internally pads the array to the next power of two, so a
-- poor fit for some array sizes.

local let log2 (n: i32) : i32 =
  let r = 0
  let (r, _) = loop (r,n) while 1 < n do
    let n = n / 2
    let r = r + 1
    in (r,n)
  in r

local let ensure_pow_2 [n] 't ((<=): t -> t -> bool) (data: [n]t): (*[]t, i32) =
  if n == 0 then (copy data, 0) else
  let d = log2 n
  in if n == 2**d
     then (copy data, d)
     else let largest = reduce (\x y -> if x <= y then y else x) data[0] data
          in (concat data (replicate (2**(d+1) - n) largest),
              d+1)

local let kernel_par [n] 't ((<=): t -> t -> bool) (a: *[n]t) (p: i32) (q: i32) : *[n]t =
  let d = 1 << (p-q) in
  map (\i -> unsafe
             let a_i = a[i]
             let up1 = ((i >> p) & 2) == 0
             in
             if (i & d) == 0
             then let a_iord = a[i | d] in
                  if a_iord <= a_i == up1
                  then a_iord else a_i
             else let a_ixord = a[i ^ d] in
                      if a_i <= a_ixord == up1
                      then a_ixord else a_i)
      (iota n)

-- | Sort an array in increasing order.
let merge_sort [n] 't ((<=): t -> t -> bool) (data: [n]t): *[n]t =
  -- We need to pad the array so that its size is a power of 2.  We do
  -- this by first finding the largest element in the input, and then
  -- using that for the padding.  Then we know that the padding will
  -- all be at the end, so we can easily cut it off.
  let (data, d) = ensure_pow_2 (<=) data
  in (loop data for i < d do
        loop data for j < i+1 do kernel_par (<=) data i j)[:n]
