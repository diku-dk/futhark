entry main (k : i64) : []i64 =
  let y = k + 5
  let x = if y < 5 then
    let a = map (+1) (iota 10)
    let b = map (+2) a
    let c = map (+3) b
    in c
  else replicate 10 10
  in x


-- entry main (k : i64) : []i64 =
--     let a = map (+1) (iota 10)
--     let b = map (+2) a
--     let c = map (+k) b
--   in c
