-- ==
-- entry: main
-- nobench input  { [-5i64,-3i64,4i64,2i64,0i64,-1i64,3i64,1i64] }
-- output         { [-1i64,-1i64,6i64,1i64,0i64,-1i64,3i64,0i64] }
-- nobench input  { [ 1i64, 2i64, 3i64, 4i64, 5i64] }
-- output         { [ 0i64, 1i64, 3i64, 6i64,10i64] }
-- nobench input  { [-1i64,-2i64,-3i64,-4i64,-5i64] }
-- output         { [-1i64,-1i64,-1i64,-1i64,-1i64] }
-- nobench input  { empty([0]i64) }
-- output         { empty([0]i64) }

#[noinline]
let foo (x : i64) =
  let ys = iota (i64.abs x)
   in if x < 0 then -1 else reduce (+) 0 ys

def main [n] (xs : [n]i64) = map foo xs
