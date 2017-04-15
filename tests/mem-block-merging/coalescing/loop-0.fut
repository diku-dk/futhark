-- Very Simple Example of Loop Coalescing.
-- ==
-- input {  [ [1,2], [3,4] ]
--          [1,2]
--       }
-- output {
--          [ [1i32, 9i32], [3i32, 4i32] ]
--        }

-- Code below should result in 4 successful coalescing
-- operations in the memory block of `y`. These are:
-- (i) @y[n/2] = a1@, (ii) `a1 = implicit-copy(x2)`,
-- (iii) `a1 = inner-loop-a1`, (iv) `a1 = a0`.
-- Note that the coalescing of `x1` in `x2 = copy x1`
-- should FAIL, because its computation requires `a1`,
-- hence `x1` cannot be coalesced in the memory block
-- of `y`, i.e., getting 5 successful coalescing 
-- operations would be incorrect! This last memory reuse
-- can potentially be done by linear-scan register 
-- allocation later!
let main(y: *[#n][#m]i32, a : [#m]i32): *[n][m]i32 =
  let y[0,1] = 9
  let a0 = copy(a)
  loop(a1 = a0) = for i < n do
    let x1 = map (+1) a1
    let x2 = copy x1
    in x2
 
  let y[n/2] = a1
  in  y
