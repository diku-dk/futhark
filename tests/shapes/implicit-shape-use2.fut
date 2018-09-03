-- ==
-- input {
--   [[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0,1.0]]
--   [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
-- }
-- output {
--     [[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0],[2.0,2.0,2.0,2.0,2.0]]
-- }

let combineVs [num_und]
             (n_row:   [num_und]f64)
             (vol_row: [num_und]f64): [num_und]f64 =
  map2 (*) n_row vol_row

let mkPrices [num_dates][num_und]
            (md_vols: [num_dates][num_und]f64,
             noises:  [num_dates][num_und]f64
            ): [num_dates][num_und]f64 =
  map2 combineVs noises md_vols

let main (vol: [][]f64) (noises: [][]f64): [][]f64 = mkPrices(vol,noises)
