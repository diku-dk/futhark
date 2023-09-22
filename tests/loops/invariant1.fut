-- Not actually invariant if you look carefully!
-- ==
-- input { 0 } output { 0 false }
-- input { 4 } output { 3 true }

entry main (n: i32) =
  let res =
    loop (x, y) = (0i32, false)
    for _i < n do
    let x' = if y then x + 1 else x
    let y' = y || true
    in  (x', y')
  in res
