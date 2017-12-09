-- Applying tiling inside of fused scanomaps.
-- ==
-- structure distributed { Kernel 4 }

let main (xs: []i32) =
  scan (+) 0 (map (\x -> reduce (+) 0 (map (+x) xs)) xs)
