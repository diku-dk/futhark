-- Left-side operands should be evaluated before before right-hand
-- operands.
-- ==
-- input { 2 } output { [[2,2,2]] }

let f (x: i32) : [][]i32 =
  [replicate (x+1) 0]

let main x =
  f x |> map (map (+2))
