-- ==
-- structure gpu { SegMap 2 }

def main (b1: bool) (b2: bool) (xs: [3]i32) (ys: [3]i32) =
  map (\x ->
         if b1
         then map (\y ->
                     if b2
                     then (map (+ (x + y)) ys, xs)
                     else (xs, ys))
                  ys
         else replicate 3 (ys, xs))
      xs
  |> map unzip
  |> unzip
