

-- entry main (k : i64) : []i64 =
--   let a = iota 10
--   let b = map (+k) a
--   let c = map (id) b
--   in c


-- entry main (k : i64) : i64 =
--   let z1 = iota 10
--   let z2 = map (+1) z1
--   let a = reduce (+) 0 z2
--   in a



--
-- entry main (k : i64) : (i64, []i64) =
--   let z1 = iota 10
--   let z2 = map (\(a, b) -> (a+1,b+2)) (zip z1 z1)
--   let (z3, z4) = unzip z2
--   let a = reduce (\a b -> a + b) 0 z3
--   in (a, z4)

entry main (k : i64) : (i64, []i64) =
  let z = iota 10
  let a = scan (+) 0 z
  let b = reduce (+) 0 a
  let a2 = map (+b) a
  in (b, a2)
