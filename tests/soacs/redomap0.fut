-- This program does not contain a redomap on its own, but fusion will
-- give rise to one.

def grayCode (x: i32) : i32 =
  (x >> 1) ^ x

def testBit (n: i32, ind: i32) : bool =
  let t = (1 << ind) in (n & t) == t

def main [num_bits] (n: i64, dir_vs: [num_bits]i32) : i32 =
  let reldv_vals =
    map (\(dv, i) : i32 ->
           if testBit (grayCode (i32.i64 n), i)
           then dv
           else 0)
        (zip (dir_vs) (map i32.i64 (iota (num_bits))))
  in reduce (^) 0 (reldv_vals)
