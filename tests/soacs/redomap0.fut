-- This program does not contain a redomap on its own, but fusion will
-- give rise to one.

fun grayCode(x: int): int =
  (x >> 1) ^ x

fun testBit(n: int, ind: int): bool =
  let t = (1 << ind) in (n & t) == t

fun main(n: int, dir_vs: [num_bits]int): int =
  let reldv_vals = map (fn (dv,i): int  =>
                          if testBit(grayCode(n),i)
                          then dv else 0
                      ) (zip (dir_vs) (iota(num_bits)) ) in
  reduce (^) 0 (reldv_vals )
