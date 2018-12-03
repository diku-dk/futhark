-- iota can be mapped.
-- ==
-- tags { no_opencl no_vulkan }
-- input { [2,2] } output { [[0,1],[0,1]] }
-- input { [2,1] } error: .

let main(ns: []i32) = map iota ns
