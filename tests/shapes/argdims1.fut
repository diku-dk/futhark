-- ==
-- input { 2 }
-- output { [0] [-1] }

let main (n: i32) =
  let foo = iota (n-1)
  let bar = replicate (n-1) (-1)
  in (foo, bar)
