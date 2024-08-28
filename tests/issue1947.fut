def consume (a: *[]i64) = a with [0] = 0
entry test (a: *[2]i64) (_: i64) n =
  let b = map id a :> [n]i64
  let a_consumed = consume a
  let final = consume (copy (filter (\x -> x > 2) a_consumed))
  in (all (\x -> x == 0) b, final)
