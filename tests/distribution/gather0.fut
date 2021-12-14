-- Sometimes it is quite important that the parallelism in an array
-- indexing result is exploited.
-- ==
-- structure gpu { SegMap 2 }

def main (is: []i32) (xss: [][]f32) =
  map (\i -> xss[i]) is
