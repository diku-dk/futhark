-- This program does not contain a redomap on its own, but fusion will
-- give rise to one.

let grayCode(x: i32): i32 =
  (x >> 1) ^ x

let testBit(n: i32, ind: i32): bool =
  let t = (1 << ind) in (n & t) == t

let main(n: i32, dir_vs: [num_bits]i32): i32 =
  let reldv_vals = map (\(dv,i): i32  ->
                          if testBit(grayCode(n),i)
                          then dv else 0
                      ) (zip (dir_vs) (iota(num_bits)) ) in
  reduce (^) 0 (reldv_vals )
