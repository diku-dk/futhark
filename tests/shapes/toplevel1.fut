-- Using a top level size.
-- When this program failed, the problem was actually in the array literal.

let n: i32 = 20
let main (xs: []i32) =
  let ys = take n xs
  in [ys]
