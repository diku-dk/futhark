-- ==
-- structure { Screma 2 }

def main =
  map (\(x: i64) ->
         #[unroll]
         loop arr = replicate 10 x
         for i < 20 do
           map2 (+) arr (indices arr))
