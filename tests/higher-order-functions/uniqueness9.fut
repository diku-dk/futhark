-- This requires care to maintain the right uniqueness attributes.

let singleton (f: i32 -> []i32): ([]i32, *[]i32) =
  let xs = f 1
  in (xs, [1])

let main (xs: []i32) =
  singleton (\y -> if y >= 0 then [xs[y]] else xs)
