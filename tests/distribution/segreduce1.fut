-- Multi-versioning of nested segmented reductions.
-- ==
-- structure gpu { SegRed 7 }

def main =
  map (map (map (map i32.product >-> i32.sum) >-> i32.minimum) >-> i32.maximum)
