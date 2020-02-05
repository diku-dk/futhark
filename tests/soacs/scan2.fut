-- Fails if the scan is rewritten to be in-place (consume md_st)
-- during first-order transform.
--
-- This compiles to a scan of a two-dimensional array, which is
-- presently not supported in the OpenCL code generator.
--
-- ==
-- tags { no_opencl }
-- input {
--   [[[1.0]]]
--   [[1.0],[2.0]]
-- }
-- output {
--   2.000000
-- }

let main (bb_mat: [][][]f64) (e_rows: [][]f64): f64 =
  let md_st = [42.0]
  let a = map (\(bb_arr_431: [][]f64) ->
                 scan (\(x_657: []f64) (y_658: []f64) -> [2.0])
                      md_st e_rows)
              bb_mat in
  a[0,0,0]
