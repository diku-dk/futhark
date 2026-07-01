-- ==
-- entry: main
-- nobench input  { [-1i64,1i64,-2i64,2i64,-3i64,3i64] }
-- output         { [ 1i64,2i64, 4i64,4i64, 9i64,6i64] }
-- nobench input  { [-5i64,-3i64,4i64,2i64,0i64,-1i64,3i64,1i64] }
-- output         { [25i64, 9i64,8i64,4i64,0i64, 1i64,6i64,2i64] }
-- nobench input  { [ 1i64, 2i64, 3i64, 4i64, 5i64] }
-- output         { [ 2i64, 4i64, 6i64, 8i64,10i64] }
-- nobench input  { [-1i64,-2i64,-3i64,-4i64,-5i64] }
-- output         { [ 1i64, 4i64, 9i64,16i64,25i64] }
-- nobench input  { empty([0]i64) }
-- output         { empty([0]i64) }

#[noinline]
let foo (x : i64) = if x < 0 then x * x else x * 2

def main [n] (xs : [n]i64) = map foo xs
