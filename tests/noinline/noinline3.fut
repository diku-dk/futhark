-- ==
-- tags { no_opencl no_cuda no_pyopencl }
-- input { [1,2,3] [0,0,1] } output { [1,1,2] }
-- structure gpu { SegMap/Apply 1 }

let f (xs: []i32) i = xs[i]

let main xs is = map (\i -> #[noinline] f xs i) is
