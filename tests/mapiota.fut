-- iota can be mapped.
-- ==
-- tags { no_opencl }
-- error: existentially sized type

let main(ns: []i32) = map iota ns
