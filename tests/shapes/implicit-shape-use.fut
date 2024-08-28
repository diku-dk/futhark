-- Extraction from generic pricer.  Uses shape declarations in ways
-- that were at one point problematic.
--
-- ==
-- input {
--   3i64
--   [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]]
--   [1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]
-- }
-- output {
--   [[109.0, 140.0, 171.0],
--    [-109.0, -140.0, -171.0],
--    [0.0, 0.0, 0.0],
--    [0.0, 0.0, 0.0],
--    [0.0, 0.0, 0.0]]
-- }

def brownianBridgeDates [num_dates]
                        (bb_inds: [3][num_dates]i32)
                        (bb_data: [3][num_dates]f64)
                        (gauss: [num_dates]f64): [num_dates]f64 =
    let bi = bb_inds[0]
    let li = bb_inds[1]
    let ri = bb_inds[2]
    let sd = bb_data[0]
    let lw = bb_data[1]
    let rw = bb_data[2]

    let bbrow = replicate num_dates 0.0
    let bbrow[ bi[0]-1 ] = sd[0] * gauss[0] in

    let bbrow = loop (bbrow) for i < num_dates-1 do  -- use i+1 since i in 1 .. num_dates-1
            let j  = li[i+1] - 1
            let k  = ri[i+1] - 1
            let l  = bi[i+1] - 1

            let wk = bbrow[k]
            let zi = gauss[i+1]
            let tmp= rw[i+1] * wk + sd[i+1] * zi

            let bbrow[ l ] = if( j == -1)
                             then tmp
                             else tmp + lw[i+1] * bbrow[j]
            in  bbrow

        -- This can be written as map-reduce, but it
        --   needs delayed arrays to be mapped nicely!
    in loop (bbrow)
        for ii < num_dates-1 do
            let i = num_dates - (ii+1)
            let bbrow[i] = bbrow[i] - bbrow[i-1]
            in  bbrow

def brownianBridge [num_dates]
               (bb_inds: [3][num_dates]i32)
               (bb_data: [3][num_dates]f64)
               (gaussian_arr: []f64) =
    let gauss2d  = unflatten gaussian_arr
    let gauss2dT = transpose gauss2d in
      transpose (
        map (brownianBridgeDates bb_inds bb_data) gauss2dT
      )

def main [num_dates] (num_und: i64)
                     (bb_inds: [3][num_dates]i32)
                     (arr: [num_dates*num_und]f64): [][]f64 =
  let bb_data= map (\(row: []i32)  ->
                        map f64.i32 row
                  ) (bb_inds )
  let bb_mat = brownianBridge bb_inds bb_data arr
  in  bb_mat
