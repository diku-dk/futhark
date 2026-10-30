-- We must maintain the dependent size in the function returned by mkhassum.
-- ==
-- input { 0i32 }
-- output { 6.0f32 }

type^ hassum = {sum: (k: i64) -> [k]f32 -> f32}

def mkhassum (x: bool) : (hassum, i32) =
  ( {sum = \(k: i64) (xs: [k]f32) -> f32.sum xs}
  , 0
  )

def usehassum (r: hassum) =
  r.sum 3 [1, 2, 3]

entry main (x: i32) =
  mkhassum true |> (.0) |> usehassum
