import "/futlib/array"

module mk_radix_sort (P: {
  type t
  val num_bits: i32
  val get_bit: i32 -> t -> i32
}): {
  val radix_sort [n]: [n]P.t -> [n]P.t
} = {
  let radix_sort_step [n] (xs: [n]P.t) (digit_n: i32): [n]P.t =
    let bits = map (P.get_bit digit_n) xs
    let bits_inv = map (1-) bits
    let ps0 = scan (+) 0 bits_inv
    let ps0_clean = map (*) bits_inv ps0
    let ps1 = scan (+) 0 bits
    let ps0_offset = reduce (+) 0 bits_inv
    let ps1_clean = map (+ps0_offset) ps1
    let ps1_clean' = map (*) bits ps1_clean
    let ps = map (+) ps0_clean ps1_clean'
    let ps_actual = map (-1) ps
    in scatter (copy xs) ps_actual xs

  let radix_sort(xs: [#n]P.t): [n]P.t =
    loop (xs) = for i < 32 do radix_sort_step xs i
    in xs
}
