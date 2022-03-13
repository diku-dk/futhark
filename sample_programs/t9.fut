

-- entry main (k : i64) : []i64 =
--   let z = iota 10
--   let a1 = map (+k) z
--   let b2 = map (+2) z
--   let r = scatter (iota 20) b2 a1
--   in r



entry main (k : i64) : []i64 =
  let z = iota 10
  let a1 = map (+k) z
  let b2 = map (+2) z
  let r = reduce_by_index (iota 20) (+) 0 b2 a1
  in r
