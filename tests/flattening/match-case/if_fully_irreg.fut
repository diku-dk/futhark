-- ==
-- entry: main
-- nobench input  { [ 2i64,  7i64,  1i64,  8i64,  7i64] }
-- output         { [ 2i64, 23i64,  0i64, 31i64, 23i64] }
-- nobench input  { [ 1i64,  2i64,  3i64,  4i64,  5i64] }
-- output         { [ 0i64,  2i64,  6i64, 12i64, 20i64] }
-- nobench input  { [ 6i64,  7i64,  8i64,  9i64, 10i64] }
-- output         { [16i64, 23i64, 31i64, 40i64, 50i64] }
-- nobench input  { empty([0]i64) }
-- output         { empty([0]i64) }

#[noinline]
let bar [n] (xs : [n]i64) =
  if n <= 5 then (false, xs)
            else (true, copy xs with [5] = n)

#[noinline]
let foo (x : i64) =
  let xs = iota x in
  let (b, ys) = bar xs
  let z = reduce (+) 0 ys
   in if b then z else z * 2

def main [n] (xs : [n]i64) = map foo xs
