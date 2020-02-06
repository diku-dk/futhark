-- ==
-- input {} error:

let predict (a:[10]f64) : i32 =
  let (m,i) = reduce (\(a,i) (b,j) -> if a > b then (a,i) else (b,j))
                       (a[9],9)
                       (zip (a[:8]) (iota 9 :> [8]i32))
  in i

let main : i32 =
  predict [0.2,0.3,0.1,0.5,0.6,0.2,0.3,0.1,0.7,0.1]
