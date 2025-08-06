-- Matrix multiplication written imperatively.  Very slow when using
-- GPU backends.
--
-- ==
-- input {
--   [ [1,2], [3,4] ]
--   [ [5,6], [7,8] ]
-- }
-- output {
--    [  [ 19 , 22  ] ,  [ 43 , 50  ]  ]
-- }
def matmult [m] [o] [n] (a: [m][o]i32, b: [o][n]i32) : [m][n]i32 =
  let res = replicate m (replicate n 0)
  in loop res for i < m do
       loop res for j < n do
         let partsum =
           loop partsum = 0
           for k < o do
             partsum + a[i, k] * b[k, j]
         let res[i, j] = partsum
         in res

def main (x: [][]i32) (y: [][]i32) : [][]i32 =
  matmult (x, y)
