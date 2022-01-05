-- A shape parameter may be used before it has been in positive
-- position at least once!
-- ==
-- input { [1,2,3] } output { [3i64,3i64,3i64] 3i64 }

def f [n] (g: i64 -> [n]i64) (xs: [n]i32) =
  let g' (x: i64) = g x : [n]i64
  in (g' (length xs), n)

def main xs = f (\x -> map (const x) xs) xs
