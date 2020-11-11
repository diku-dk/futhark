-- ==
-- entry: main
--
-- compiled random input { [26214400]i32 } auto output

local let radix_sort_step [n] 't (xs: [n]t) (get_bit: i32 -> t -> i32)
                                 (digit_n: i32): [n]t =
  let num x = get_bit (digit_n+1) x * 2 + get_bit digit_n x
  let pairwise op (a1,b1,c1,d1) (a2,b2,c2,d2) =
    (a1 `op` a2, b1 `op` b2, c1 `op` c2, d1 `op` d2)
  let bins = xs |> map num
  let flags = bins |> map (\x -> if x == 0 then (1,0,0,0)
                                 else if x == 1 then (0,1,0,0)
                                 else if x == 2 then (0,0,1,0)
                                 else (0,0,0,1))
  let offsets = scan (pairwise (+)) (0,0,0,0) flags
  let (na,nb,nc,_nd) = last offsets
  let f bin (a,b,c,d) = match bin
                        case 0 -> a-1
                        case 1 -> na+b-1
                        case 2 -> na+nb+c-1
                        case _ -> na+nb+nc+d-1
  let is = map2 f bins offsets
  in scatter (copy xs) is xs

-- | The `num_bits` and `get_bit` arguments can be taken from one of
-- the numeric modules of module type `integral`@mtype@"/futlib/math"
-- or `float`@mtype@"/futlib/math", such as `i32`@term@"/futlib/math"
-- or `f64`@term@"/futlib/math".  However, if you know that
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
let radix_sort [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                      (xs: [n]t): [n]t =
  let iters = if n == 0 then 0 else (num_bits+2-1)/2
  in loop xs for i < iters do radix_sort_step xs get_bit (i*2)

let with_indices [n] 'a (xs: [n]a) : [n](a, i32) =
  zip xs (iota n)

local let by_key_wrapper [n] 't sorter key num_bits get_bit (xs: [n]t) : [n]t =
  map key xs
  |> with_indices
  |> sorter num_bits (\i (k, _) -> get_bit i k)
  |> map (\(_, i : i32) -> xs[i]) -- OK because '0<=i<n'.

-- | Like `radix_sort`, but sort based on key function.
let radix_sort_by_key [n] 't 'k
    (key: t -> k)
    (num_bits: i32) (get_bit: i32 -> k -> i32) (xs: [n]t): [n]t =
  by_key_wrapper radix_sort key num_bits get_bit xs

-- | A thin wrapper around `radix_sort`@term that ensures negative
-- integers are sorted as expected.  Simply pass the usual `num_bits`
-- and `get_bit` definitions from e.g. `i32`@term@"/futlib/math".
let radix_sort_int [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                          (xs: [n]t): [n]t =
  let get_bit' i x =
    -- Flip the most significant bit.
    let b = get_bit i x
    in if i == num_bits-1 then b ^ 1 else b
  in radix_sort num_bits get_bit' xs

-- | Like `radix_sort_int`, but sort based on key function.
let radix_sort_int_by_key [n] 't 'k
    (key: t -> k)
    (num_bits: i32) (get_bit: i32 -> k -> i32) (xs: [n]t): [n]t =
  by_key_wrapper radix_sort_int key num_bits get_bit xs

-- | A thin wrapper around `radix_sort`@term that ensures floats are
-- sorted as expected.  Simply pass the usual `num_bits` and `get_bit`
-- definitions from `f32`@term@"/futlib/math" and
-- `f64`@term@"/futlib/math".
let radix_sort_float [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                            (xs: [n]t): [n]t =
  let get_bit' i x =
    -- We flip the bit returned if:
    --
    -- 0) the most significant bit is set (this makes more negative
    --    numbers sort before less negative numbers), or
    --
    -- 1) we are asked for the most significant bit (this makes
    --    negative numbers sort before positive numbers).
    let b = get_bit i x
    in if get_bit (num_bits-1) x == 1 || i == num_bits-1
       then b ^ 1 else b
  in radix_sort num_bits get_bit' xs

-- | Like `radix_sort_float`, but sort based on key function.
let radix_sort_float_by_key [n] 't 'k
    (key: t -> k)
    (num_bits: i32) (get_bit: i32 -> k -> i32) (xs: [n]t): [n]t =
  by_key_wrapper radix_sort_float key num_bits get_bit xs

let main [n] (xs: [n]i32): [n]i32 =
  radix_sort_int i32.num_bits i32.get_bit xs

