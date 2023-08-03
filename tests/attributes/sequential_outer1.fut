-- Slightly odd result due to interchange.
-- ==
-- random input { [10][10][10]i32 } auto output
-- structure gpu {
--   /Loop 1
--   /Loop/SegRed 1
--   /Loop/SegMap 1
-- }

def main xsss =
  #[incremental_flattening(only_inner)]
  map (\xss -> #[sequential_outer] map i32.sum xss) xsss
