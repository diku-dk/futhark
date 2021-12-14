-- When turning a map-iota into a proper map, the array being indexed
-- does not have to be of the same size as the map.
-- ==
-- input { 3i64 [1,2,3] } output { [1,4,9] }
-- structure { Screma 1 }

def main [k] (n: i64) (xs: [k]i32) =
  let ys = map (\i -> #[unsafe] xs[i]) (iota n)
  in map (\i -> ys[i] * xs[i]) (iota n)
