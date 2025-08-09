-- 1D tiling where the two dimensions are separated by a sequential
-- loop.  This cannot actually be tiled, because the iteration count
-- is variant to the only dimension.

def main (xs: []i32) =
  map (\x -> loop acc = 0 for i < x do i32.sum (map (+ acc) xs)) xs
