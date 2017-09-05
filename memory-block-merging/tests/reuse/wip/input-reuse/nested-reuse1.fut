-- Similar to nested-reuse0.fut, but with a unique, reusable input.  This works
-- for the GPU pipeline, which results in 0 allocations, but does not work for
-- the CPU pipeline, which gets 2 allocations like in nested-reuse0.fut.  FIXME:
-- This is because the relationship between the input xss_mem and the output
-- memory is less clear when the kernel has been transformed into nested loops.
-- For now we focus on kernels.
-- ==
-- input { [[2, 5, 9], [1, 2, 3]] }
-- output { [[6, 9, 13], [4, 5, 6]] }
-- structure gpu { Alloc 0 }

-- structure cpu { Alloc 1 }

let main (xss: *[][]i32): [][]i32 =
  map (\xs ->
       let ys = map (+ 1) xs
       let k = ys[0]
       let zs = map (+ k) ys -- Can reuse the memory of ys.
       in zs) xss
