-- | A non-comparison-based sort that sorts an array in *O(k n)* work
-- and *O(k)* span, where *k* is the number of bits in each element.
--
-- Generally, this is the sorting function we recommend for Futhark
-- programs.

local let radix_sort_step [n] 't (xs: [n]t) (get_bit: i32 -> t -> i32)
                                 (digit_n: i32): [n]t =
  let bits = map (get_bit digit_n) xs
  let bits_inv = map (1-) bits
  let ps0 = scan (+) 0 bits_inv
  let ps0_clean = map2 (*) bits_inv ps0
  let ps1 = scan (+) 0 bits
  let ps0_offset = reduce (+) 0 bits_inv
  let ps1_clean = map (+ps0_offset) ps1
  let ps1_clean' = map2 (*) bits ps1_clean
  let ps = map2 (+) ps0_clean ps1_clean'
  let ps_actual = map (\x -> x-1) ps
  in scatter (copy xs) ps_actual xs

-- | The `num_bits` and `get_bit` arguments can be taken from one of
-- the numeric modules, such as `i32`@term.  However, if you know that
-- the input array only uses lower-order bits (say, if all integers
-- are less than 100), then you can profitably pass a smaller
-- `num_bits` value to reduce the number of sequential iterations.
let radix_sort [n] 't (num_bits: i32) (get_bit: i32 -> t -> i32)
                      (xs: [n]t): [n]t =
  loop xs for i < num_bits do radix_sort_step xs get_bit i
