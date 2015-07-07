-- ==
-- input {
--   {
--     [[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0]]
--   , [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
--   }
-- }
-- output {
--     [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
-- }

fun [real,num_und] combineVs(  [real,num_und] n_row,
                               [real,num_und] vol_row
) =
    map(*, zip(n_row, vol_row ) )

fun [[real,num_und],num_dates]
mkPrices(  [[real,num_und],num_dates] md_vols,
           [[real,num_und],num_dates] noises
) =
    map( combineVs, zip(noises, md_vols) )


fun [[real]] main([[real]] vol, [[real]] noises) = mkPrices(vol,noises)
