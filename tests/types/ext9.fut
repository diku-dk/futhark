-- ==
-- input { [1,2,3] }
-- output { 2i64 3i64 }

def f [n] [m] 't (_: [n + m]t) = (n, m)

def main [n] (xs: [n]i32) =
  f (let xs' = filter (< 3) xs
     in xs' ++ xs)
