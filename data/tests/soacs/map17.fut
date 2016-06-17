-- Derived from OptionPricing.  Miscompiled with OpenCL backend due to
-- erroneous allocation expansion.
-- ==
-- input {
--  [[1],[0],[0]]
--  [[ 0.9889803798765787 ],
--   [ 0.0000000000000000 ],
--   [ 0.0000000000000000 ]]
--  [[[0.000000]], [[0.674490]]]
-- }
-- output { [[[0.000000]], [[0.674490]]] }

fun [num_dates]f64 doInPlaceUpdate([3][num_dates]int bb_inds,
                                     [3][num_dates]f64 bb_data,
                                     [num_dates]f64     gauss) =
    let bbrow = replicate(num_dates, 0.0)   in
    let bbrow[ 0 ] = gauss[0]               in
    bbrow

fun [][]f64 mapInPlaceUpdate([3][num_dates]int bb_inds,
                              [3][num_dates]f64 bb_data,
                              [num_und][num_dates]f64  gauss2dT) =
  map(doInPlaceUpdate(bb_inds, bb_data), gauss2dT)

----------------------------------------
-- MAIN
----------------------------------------

fun [][][]f64 main([3][num_dates]int         bb_inds,
                    [3][num_dates]f64         bb_data,
                    [][num_und][num_dates]f64 gauss_mat) =
  map ( mapInPlaceUpdate( bb_inds, bb_data ), gauss_mat )
