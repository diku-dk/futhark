-- Extraction from generic pricer.  Uses shape declarations in ways
-- that were at one point problematic.
--
-- ==
-- input {
--   3
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

let brownianBridgeDates [num_dates]
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
            unsafe
            let j  = li[i+1] - 1
            let k  = ri[i+1] - 1
            let l  = bi[i+1] - 1

            let wk = unsafe bbrow[k]
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

let brownianBridge [num_dates]
               (num_und:
                i32,
                bb_inds: [3][num_dates]i32,
                bb_data: [3][num_dates]f64,
                 gaussian_arr: []f64
            ) =
    let gauss2d  = unflatten num_dates num_und gaussian_arr
    let gauss2dT = transpose gauss2d in
      transpose (
        map (brownianBridgeDates bb_inds bb_data) gauss2dT
      )

let main [num_dates] (num_und: i32)
                     (bb_inds: [3][num_dates]i32)
                     (arr_usz: []f64): [][]f64 =
  let n = num_dates*num_und
  let arr    = arr_usz :> [n]f64
  let bb_data= map (\(row: []i32)  ->
                        map r64 row
                  ) (bb_inds )
  let bb_mat = brownianBridge( num_und, bb_inds, bb_data, arr )
  in  bb_mat
