
-- entry main (k : i64) : [10]([10]i64, i64) =
--   let a = zip (map (\_ -> iota 10) (iota 10)) (iota 10)
--   let b' = [6,7]
--   let b = map (+1) b'
--   let c = [([9,8,7,6,5,4,3,2,1,0], 15),([8,8,7,6,5,4,3,2,1,1], 16)]
--   let d = scatter a b c
--   in d
--

-- c should be fusibles


-- entry main (k : i64) : i64 =
--   let a = zip (map (\_ -> iota 10) (iota 10)) (iota 10)
--   let b = [6,7]
--   let d = scatter a b c


let main (is: []i32) (xs': []i32) =
  let xs = map (+1) xs'
  let foo = (map (+1) xs)[0]
  let bar = (map (+2) xs)[0]
  in (foo, bar)
