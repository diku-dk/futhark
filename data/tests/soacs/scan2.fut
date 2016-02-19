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

fun real main([[[real]]] bb_mat, [[real]] e_rows) =
  let md_st = [42.0] in
  let a = map(fn [[real]] ([[real]] bb_arr_431) =>
                scan(fn [real] ([real] x_657, [real] y_658) => [2.0],
                     md_st, e_rows)
             , bb_mat) in
  a[0,0,0]
