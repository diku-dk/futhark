-- Test2 Memory-Block Merging: Coalescing transitive in-place update
-- ==
-- input { 
--          [3, 5, 7]
--          [  [ [1,2,3], [4,5,6], [7,8,9] ]
--          ,  [ [1,2,3], [4,5,6], [7,8,9] ]
--          ,  [ [1,2,3], [4,5,6], [7,8,9] ]
--          ] 
--       }
-- output { 
--          [  [ [1,2,3], [4,5,6], [7,8,9] ]
--          ,  [ [4,6,8], [5,5,5], [7,7,7] ]
--          ,  [ [6,6,6], [10,10,10], [14,14,14] ]
--          ] 
--        }

-- Should result in 3 successful coalescing operations (all).
let main(a: [#n]i32, y: *[#n][#n][#n]i32): [n][n][n]i32 =
  let x = map (\i -> replicate n i) a
  let b = map (+1) a
  let x[n-3] = b
  let y[n-2] = x
  let c      = map (\i -> replicate n (2*i)) a
  let y[n-1] = c
  in  y
