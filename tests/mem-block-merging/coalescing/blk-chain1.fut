-- Test1 Memory-Block Merging
-- ==
-- input { [0, 3, 5, 7, 9] }
-- output { [[0i32, 0i32, 0i32, 0i32, 0i32],[6i32, 6i32, 6i32, 6i32, 6i32],[10i32, 10i32, 10i32, 10i32, 10i32],[14i32, 14i32, 14i32, 14i32, 14i32],[18i32, 18i32, 18i32, 18i32, 18i32],[0i32, 0i32, 0i32, 0i32, 0i32],[3i32, 3i32, 3i32, 3i32, 3i32],[1i32, 4i32, 6i32, 8i32, 10i32],[7i32, 7i32, 7i32, 7i32, 7i32],[9i32, 9i32, 9i32, 9i32, 9i32]] }

-- Without hoisting allocations, there is only 
-- one coalescing opportunity, i.e., `x[2] = b`.
-- The other one `y = concat a2 x` would only 
-- succeed if the allocation of `y` is hoisted
-- before the creation of `x` and `a2` 

let main(a: [#n]i32): [][n]i32 =
  let x = map (\i -> replicate n i) a
--  in  copy(x)
  let b = map (+1) a
  let x[2] = b
  let a2= map (\i -> replicate n (2*i)) a
  let y = concat a2 x
  in  y
