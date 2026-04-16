-- | A non-comparison-based sort that sorts an array in *O(k n)* work
-- and *O(k log(n))* span, where *k* is the number of bits in each element.
--
-- Generally, this is the sorting function we recommend for Futhark
-- programs, but be careful about negative integers (use
-- `radix_sort_int`@term) and floating-point numbers (use
-- `radix_sort_float`@term).  If you need a comparison-based sort,
-- consider `merge_sort`@term@"merge_sort".
--
-- ## See Also
--
-- * `merge_sort`@term@"merge_sort"

local
def radix_sort_step [n] 't
                    (xs: [n]t)
                                  (get_bit: i32 -> t -> i32)
                                  (digit_n: i32) : [n]t =
  let num x = i8.i32 <| get_bit (digit_n + 1) x * 2 + get_bit digit_n x
  let pairwise op (a1, b1, c1, d1) (a2, b2, c2, d2) = (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)
  let pairwise' op (a1, b1, c1) (a2, b2, c2) = (a1 `op` a2, b1 `op` b2, c1 `op` c2)
  let bins = map num xs
  let flags =
    bins
    |> map (\x -> (x == 0, x == 1, x == 2, x == 3))
  let flags' =
    map num xs
    |> map (\x ->
              ( i64.bool (x == 0)
              , i64.bool (x == 1)
              , i64.bool (x == 2)
              ))
  let offsets =
    flags
    |> map (\(a, b, c, d) -> (i64.bool a, i64.bool b, i64.bool c, i64.bool d))
    |> scan (pairwise (+)) (0, 0, 0, 0)
  let (na, nb, nc) = reduce_comm (pairwise' (+)) (0, 0, 0) flags'
  let f bin (a, b, c, d) =
    (-1)
    + a * (i64.bool (bin == 0))
    + na * (i64.bool (bin > 0))
    + b * (i64.bool (bin == 1))
    + nb * (i64.bool (bin > 1))
    + c * (i64.bool (bin == 2))
    + nc * (i64.bool (bin > 2))
    + d * (i64.bool (bin == 3))
  let is = map2 f bins offsets
  in scatter (#[scratch] copy xs) is xs

-- | The `num_bits` and `get_bit` arguments can be taken from one of
-- the numeric modules of module type `integral`@mtype@"/prelude/math"
-- or `float`@mtype@"/prelude/math", such as `i32`@term@"/prelude/math"
-- or `f64`@term@"/prelude/math".  However, if you know that
-- the input array only uses lower-order bits (say, if all integers
-- are less than 100), then you can profitably pass a smaller
-- `num_bits` value to reduce the number of sequential iterations.
--
-- **Warning:** while radix sort can be used with numbers, the bitwise
-- representation of of both integers and floats means that negative
-- numbers are sorted as *greater* than non-negative.  Negative floats
-- are further sorted according to their absolute value.  For example,
-- radix-sorting `[-2.0, -1.0, 0.0, 1.0, 2.0]` will produce `[0.0,
-- 1.0, 2.0, -1.0, -2.0]`.  Use `radix_sort_int`@term and
-- `radix_sort_float`@term in the (likely) cases that this is not what
-- you want.
def radix_sort [n] 't
               (num_bits: i32)
               (get_bit: i32 -> t -> i32)
               (xs: [n]t) : [n]t =
  let iters = if n == 0 then 0 else (num_bits + 2 - 1) / 2
  in loop xs for i < iters do radix_sort_step xs get_bit (i * 2)

def with_indices [n] 'a (xs: [n]a) : [n](a, i64) =
  zip xs (iota n)

local
def by_key_wrapper [n] 't sorter key num_bits get_bit (xs: [n]t) : [n]t =
  map key xs
  |> with_indices
  |> sorter num_bits (\i (k, _) -> get_bit i k)
  |> map (\(_, i: i64) -> xs[i])

-- OK because '0<=i<n'.

-- | Like `radix_sort`, but sort based on key function.
def radix_sort_by_key [n] 't 'k
                      (key: t -> k)
                      (num_bits: i32)
                      (get_bit: i32 -> k -> i32)
                      (xs: [n]t) : [n]t =
  by_key_wrapper radix_sort key num_bits get_bit xs

-- | A thin wrapper around `radix_sort`@term that ensures negative
-- integers are sorted as expected.  Simply pass the usual `num_bits`
-- and `get_bit` definitions from e.g. `i32`@term@"/prelude/math".
def radix_sort_int [n] 't
                   (num_bits: i32)
                   (get_bit: i32 -> t -> i32)
                   (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits - 1 then b ^ 1 else b
  in radix_sort num_bits get_bit' xs

-- | Like `radix_sort_int`, but sort based on key function.
def radix_sort_int_by_key [n] 't 'k
                          (key: t -> k)
                          (num_bits: i32)
                          (get_bit: i32 -> k -> i32)
                          (xs: [n]t) : [n]t =
  by_key_wrapper radix_sort_int key num_bits get_bit xs

-- | A thin wrapper around `radix_sort`@term that ensures floats are
-- sorted as expected.  Simply pass the usual `num_bits` and `get_bit`
-- definitions from `f32`@term@"/prelude/math" and
-- `f64`@term@"/prelude/math".
def radix_sort_float [n] 't
                     (num_bits: i32)
                     (get_bit: i32 -> t -> i32)
                     (xs: [n]t) : [n]t =
  let get_bit' i x =
    -- We flip the bit returned if:
    --
    -- 0) the most significant bit is set (this makes more negative
    --    numbers sort before less negative numbers), or
    --
    -- 1) we are asked for the most significant bit (this makes
    --    negative numbers sort before positive numbers).
    let b = get_bit i x
    in if get_bit (num_bits - 1) x == 1 || i == num_bits - 1
       then b ^ 1
       else b
  in radix_sort num_bits get_bit' xs

-- | Like `radix_sort_float`, but sort based on key function.
def radix_sort_float_by_key [n] 't 'k
                            (key: t -> k)
                            (num_bits: i32)
                            (get_bit: i32 -> k -> i32)
                            (xs: [n]t) : [n]t =
  by_key_wrapper radix_sort_float key num_bits get_bit xs
