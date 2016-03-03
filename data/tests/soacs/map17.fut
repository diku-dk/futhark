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

fun [f64,num_dates] doInPlaceUpdate([[int, num_dates],3] bb_inds,
                                     [[f64,num_dates],3] bb_data,
                                     [f64,num_dates]     gauss) =
    let bbrow = replicate(num_dates, 0.0)   in
    let bbrow[ 0 ] = gauss[0]               in
    bbrow

fun [[f64]] mapInPlaceUpdate([[int, num_dates],3] bb_inds,
                              [[f64,num_dates],3] bb_data,
                              [[f64,num_dates],num_und]  gauss2dT) =
  map(doInPlaceUpdate(bb_inds, bb_data), gauss2dT)

----------------------------------------
-- MAIN
----------------------------------------

fun [[[f64]]] main([[int, num_dates],3]         bb_inds,
                    [[f64,num_dates],3]         bb_data,
                    [[[f64,num_dates],num_und]] gauss_mat) =
  map ( mapInPlaceUpdate( bb_inds, bb_data ), gauss_mat )
