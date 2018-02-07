-- Memory created inside a map/loop and is used in sub-maps/loops can have a
-- reuse.
--
-- Note that this is run without coalescing, which is how the last array in the
-- map can reuse the memory of the first: Both arrays in the map use memory only
-- in use inside the body.  With coalescing, the last array would use the final
-- output memory, but without coalescing that happens through a copy.
--
-- For the CPU pipeline, where will be one memory block for the result as a
-- whole, and one memory block for the intermediate arrays in the body.  For the
-- GPU pipeline, the program will be distributed into two kernels, so there will
-- only be one allocation (this is really a test of the CPU pipeline).
-- ==
-- input { [[2, 5, 9], [1, 2, 3]] }
-- output { [[6, 9, 13], [4, 5, 6]] }
-- structure cpu { Alloc 2 }
-- structure gpu { Alloc 1 }

let main (xss: [][]i32): [][]i32 =
  map (\xs ->
       let ys = map (+ 1) xs
       let k = ys[0]
       let zs = map (+ k) ys -- Can reuse the memory of ys.
       in zs) xss
