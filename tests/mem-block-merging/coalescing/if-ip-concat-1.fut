-- Very Simple Coalescing for if.
-- ==
-- input {  [ [1i32, 1i32, 1i32, 1i32]
--          , [1i32, 1i32, 1i32, 1i32] 
--          ]
--          [3, 7]
--          [8, 9]
--       }
-- output { 
--          [ [4i32, 8i32, 9i32, 10i32]
--          , [1i32, 1i32, 1i32, 1i32 ] 
--          ]
--        }

-- Should result in 7 successful coalescing operations:
-- `y[0] = z`, `z = r1`, `z = r2`, `r1 = concat a1 b1`,
-- and `r2 = concat a2 b2`, i.e., one for each 
-- `a1`, `b1`, `a2`, `b2`. 
-- The read from `y` should not disable coalescing of
-- `y[0] = z` because it occurs before `z` is actually
-- created (not in the lifespan of `z`).
fun main(y : *[n][q]i32, a : [n]i32, b : [m]i32): *[n][q]i32 =
--  let y = replicate n (replicate (n+m) 1i32)
  let z = if (y[0,0]) > 0 
          then let a1 = map (+y[0,0]) a
               let b1 = map (+1) b
               let r1 = concat a1 b1
               in  r1
          else let b2 = map (*2) b
               let a2 = map (*3) a
               let r2 = concat b2 a2
                        --if n > 0
                        --then let r2 = copy(x2)
                        --     in  r2
                        --else x2
               in  r2
  let y[0] = z
  in  y
