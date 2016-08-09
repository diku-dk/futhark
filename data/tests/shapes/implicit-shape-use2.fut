-- ==
-- input {
--   [[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0]]
--   [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
-- }
-- output {
--     [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
-- }

fun [num_und]f64 combineVs(  [num_und]f64 n_row,
                               [num_und]f64 vol_row
) =
    map(*, zip(n_row, vol_row ) )

fun [num_dates][num_und]f64
mkPrices(  [num_dates][num_und]f64 md_vols,
           [num_dates][num_und]f64 noises
) =
    map( combineVs, zip(noises, md_vols) )


fun [][]f64 main([][]f64 vol, [][]f64 noises) = mkPrices(vol,noises)
