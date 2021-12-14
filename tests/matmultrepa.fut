-- Matrix multiplication written in a Repa-like style.
-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }
-- compiled random input { [16][16]i32 [16][16]i32 } auto output
-- compiled random input { [16][32]i32 [32][16]i32 } auto output
-- compiled random input { [32][16]i32 [16][32]i32 } auto output
-- compiled random input { [31][32]i32 [32][31]i32 } auto output
-- compiled random input { [128][17]i32 [17][128]i32 } auto output
-- structure { /Screma 1 /Screma/Screma 1 Screma/Screma/Screma 1 }
def redplus1(a: []i32): i32 = reduce (+) 0 a
def redplus2 [n][m] (a: [n][m]i32): [n]i32 = map redplus1 a

def mul1 [m]    (a: [m]i32, b: [m]i32): [m]i32 = map2 (*) a b
def mul2 [n][m] (a: [n][m]i32, b: [n][m]i32): [n][m]i32 = map mul1 (zip a b)

def replin [m] (n: i64) (a: [m]i32): [n][m]i32 = replicate n a

def matmultFun [n][m] (a: [n][m]i32, b: [m][n]i32 ): [n][n]i32 =
    let br  = replicate n (transpose b)
    let ar  = map       (replin n) a
    let abr = map   mul2 (zip ar br)
    in map redplus2 abr

def main [n][m] (x: [n][m]i32) (y: [m][n]i32): [n][n]i32 =
  matmultFun(x, y)
