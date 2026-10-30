-- ==
-- input {
--   [[1.0f32,1.0f32,1.0f32,1.0f32,1.0f32],[1.0f32,1.0f32,1.0f32,1.0f32,1.0f32],[1.0f32,1.0f32,1.0f32,1.0f32,1.0f32]]
--   [[2.0f32,2.0f32,2.0f32,2.0f32,2.0f32],[2.0f32,2.0f32,2.0f32,2.0f32,2.0f32],[2.0f32,2.0f32,2.0f32,2.0f32,2.0f32]]
-- }
-- output {
--     [[2.0f32,2.0f32,2.0f32,2.0f32,2.0f32],[2.0f32,2.0f32,2.0f32,2.0f32,2.0f32],[2.0f32,2.0f32,2.0f32,2.0f32,2.0f32]]
-- }

def combineVs [num_und]
              (n_row: [num_und]f32)
              (vol_row: [num_und]f32) : [num_und]f32 =
  map2 (*) n_row vol_row

def mkPrices [num_dates] [num_und]
             ( md_vols: [num_dates][num_und]f32
             , noises: [num_dates][num_und]f32
             ) : [num_dates][num_und]f32 =
  map2 combineVs noises md_vols

def main (vol: [][]f32) (noises: [][]f32) : [][]f32 = mkPrices (vol, noises)
