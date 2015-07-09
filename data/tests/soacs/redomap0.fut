-- This program does not contain a redomap on its own, but fusion will
-- give rise to one.

fun int grayCode(int x) =
  (x >> 1) ^ x

fun bool testBit(int n, int ind) =
  let t = (1 << ind) in (n & t) == t

fun int main(int n, [int,num_bits] dir_vs) =
  let reldv_vals = map( fn int (int dv, int i) =>
                          if testBit(grayCode(n),i)
                          then dv else 0
                      , zip(dir_vs,iota(num_bits)) ) in
  reduce( ^, 0, reldv_vals )
