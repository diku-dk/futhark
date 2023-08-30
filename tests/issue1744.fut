-- ==
-- tags { no_opencl no_cuda no_hip no_pyopencl }

entry main (xs: [3][3]f32) : [3]f32 =
  map (\x ->
         let ss = map (map2 (*) x) xs
         let h = 0
         let (_,h) = loop (ss, h) for _ in iota 3
                     do (tail ss, f32.sum (flatten ss))
         in h)
      xs
