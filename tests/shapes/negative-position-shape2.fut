-- A shape parameter may be used before it has been in positive
-- position at least once!
-- ==
-- input { [1,2,3] } output { [3,3,3] 3 }

let f [n] (g: i32 -> [n]i32) (xs: [n]i32) =
  let g' (x: i32) = g x : [n]i32
  in (g' (length xs), n)

let main xs = f (\x -> map (const x) xs) xs
