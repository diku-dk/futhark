-- A scan of a two-dimensional array, which is presently not supported
-- in the OpenCL code generator.
--
-- ==
-- tags { no_opencl }

let segmented_scan [n] 't (op: t -> t -> t) (ne: t)
                          (flags: [n]bool) (as: [n]t): [n]t =
  (unzip (scan (\(x_flag,x) (y_flag,y) ->
                (x_flag || y_flag,
                 if y_flag then y else x `op` y))
          (false, ne)
          (zip flags as))).1

let main [n][m] (flags: [n]bool) (xss: [n][m]i32): [n][m]i32 =
  segmented_scan (map2 (+)) (replicate m 0) flags xss
