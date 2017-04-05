-- Very Simple Example of Loop Coalescing.
-- ==
-- input {  [ [1,2], [3,4] ]
--          [1,2]
--       }
-- output {
--          [ [1i32, 9i32], [3i32, 4i32] ]
--        }

fun main(y: *[#n][#m]i32, a : [#m]i32): *[n][m]i32 =
  let y[0,1] = 9
  let a0 = copy(a)
  loop(a1 = a0) = for i < n do
    let x1 = map (+1) a1
    let x2 = copy x1
    in x2
 
  let y[n/2] = a1
  in  y
