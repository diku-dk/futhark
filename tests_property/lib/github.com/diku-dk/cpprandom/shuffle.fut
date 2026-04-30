-- | Random shuffling of an array in parallel.

open import "random"

local
module type shuffle = {
  type rng

  -- | Given a source of random numbers, shuffle an array.
  val shuffle [n] 't : rng -> [n]t -> (rng, [n]t)

  -- | Given `n` sources of random numbers, shuffle an `n` element array.
  val shuffle' [n] 't : [n]rng -> [n]t -> ([n]rng, [n]t)
}

module mk_shuffle (I: integral) (E: rng_engine with t = I.t) : shuffle with rng = E.rng = {
  type rng = E.rng

  def radix_sort_step [n] 't
                      (xs: [n]t)
                      (get_bit: i32 -> t -> i32)
                      (digit_n: i32) : [n]t =
    let bits = map (get_bit digit_n) xs
    let bits_inv = map (1 -) bits
    let ps0 = scan (+) 0 bits_inv
    let ps0_clean = map2 (*) bits_inv ps0
    let ps1 = scan (+) 0 bits
    let ps0_offset = reduce (+) 0 bits_inv
    let ps1_clean = map (+ ps0_offset) ps1
    let ps1_clean' = map2 (*) bits ps1_clean
    let ps = map2 (+) ps0_clean ps1_clean'
    let ps_actual = map (\x -> x - 1) ps
    in scatter (copy xs) (map i64.i32 ps_actual) xs

  def radix_sort [n] 't
                 (num_bits: i32)
                 (get_bit: i32 -> t -> i32)
                 (xs: [n]t) : [n]t =
    loop xs for i < num_bits do radix_sort_step xs get_bit i

  module dist = uniform_int_distribution i64 I E

  def shuffle' [n] 't rngs (xs: [n]t) =
    let (rngs', keys) = map (dist.rand (0, n - 1)) rngs |> unzip
    let get_bit i x = i64.get_bit i keys[x]
    let num_bits = i32.f64 (f64.ceil (f64.log2 (f64.i64 n)))
    let xs' =
      radix_sort num_bits get_bit (iota n)
      |> map (\i -> xs[i])
    in (rngs', xs')

  def shuffle [n] 't rng (xs: [n]t) =
    let rngs = E.split_rng n rng
    let (rngs', xs') = shuffle' rngs xs
    in (E.join_rng rngs', xs')
}
