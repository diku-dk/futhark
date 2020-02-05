-- iota cannot be mapped.
-- ==
-- tags { no_opencl }
-- error: type containing anonymous sizes

let main(ns: []i32) = map iota ns
