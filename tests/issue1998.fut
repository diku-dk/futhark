-- | file: error.fut

module type bitset = {
  type bitset [n]
  val nbs : i64
  val empty : (n: i64) -> bitset [(n - 1) / nbs + 1]
  val complement [n] : bitset [(n - 1) / nbs + 1] -> bitset [(n - 1) / nbs + 1]
  val size [n] : bitset [(n - 1) / nbs + 1] -> i64
}

module mk_bitset (I: integral) : bitset = {
  def nbs = i64.i32 I.num_bits
  type bitset [n] = [n]I.t
  def zero : I.t = I.i64 0

  def empty (n: i64) : bitset [(n - 1) / nbs + 1] =
    replicate ((n - 1) / nbs + 1) zero

  def set_front_bits_zero [n] (s: bitset [(n - 1) / nbs + 1]) : bitset [(n - 1) / nbs + 1] =
    let l = (n - 1) / nbs + 1
    let start = 1 + (n - 1) % nbs
    let to_keep = I.i64 (i64.not (i64.not 0 << start))
    in if l == 0
       then s
       else copy s with [l - 1] = s[l - 1] I.& to_keep

  def complement [n] (s: bitset [(n - 1) / nbs + 1]) : bitset [(n - 1) / nbs + 1] =
    map I.not s
    |> set_front_bits_zero

  def size [n] (s: bitset [(n - 1) / nbs + 1]) : i64 =
    map (i64.i32 <-< I.popc) s
    |> i64.sum
}

module bitset_u8 = mk_bitset u8

-- ==
-- entry: test_complement
-- input { 0u8 } output { 0i64 }
-- input { 1u8 } output { 1i64 }
-- input { 2u8 } output { 2i64 }
-- input { 8u8 } output { 8i64 }
entry test_complement (c: u8) : i64 =
  let c' = i64.u8 c
  let empty_set = bitset_u8.empty c'
  let full_set = bitset_u8.complement empty_set
  let result = bitset_u8.size full_set
  in result
