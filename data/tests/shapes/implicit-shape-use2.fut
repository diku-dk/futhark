-- ==
-- input {
--   (
--     [[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0]]
--   , [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
--   )
-- }
-- output {
--     [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
-- }

fun [f64,num_und] combineVs(  [f64,num_und] n_row,
                               [f64,num_und] vol_row
) =
    map(*, zip(n_row, vol_row ) )

fun [[f64,num_und],num_dates]
mkPrices(  [[f64,num_und],num_dates] md_vols,
           [[f64,num_und],num_dates] noises
) =
    map( combineVs, zip(noises, md_vols) )


fun [[f64]] main([[f64]] vol, [[f64]] noises) = mkPrices(vol,noises)
