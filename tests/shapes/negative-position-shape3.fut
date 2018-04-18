-- A shape parameter can be used in negative position in the same
-- parameter as where the shape is first given.
-- ==
-- input { [3,5,2,1] } output { [0,1,2,3] 4 }

let f [n] (g: i32 -> [n]i32, xs: [n]i32) =
  let g' (x: i32) = g x : [n]i32
  in (g' (length xs), n)

let main xs = f (iota, xs)
