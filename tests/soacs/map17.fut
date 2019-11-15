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

let doInPlaceUpdate [num_dates]
                   (bb_inds: [3][num_dates]i32)
                   (bb_data: [3][num_dates]f64)
                   (gauss: [num_dates]f64): [num_dates]f64 =
    let bbrow = replicate num_dates 0.0
    let bbrow[ 0 ] = gauss[0]
    in bbrow

let mapInPlaceUpdate [num_dates][num_und]
                    (bb_inds: [3][num_dates]i32)
                    (bb_data: [3][num_dates]f64)
                    (gauss2dT: [num_und][num_dates]f64): [num_und][num_dates]f64 =
  map (doInPlaceUpdate bb_inds bb_data) gauss2dT

----------------------------------------
-- MAIN
----------------------------------------

let main [n][num_dates][num_und]
        (bb_inds: [3][num_dates]i32)
        (bb_data: [3][num_dates]f64)
        (gauss_mat: [n][num_und][num_dates]f64): [][][]f64 =
  map  (mapInPlaceUpdate bb_inds bb_data) (gauss_mat )
