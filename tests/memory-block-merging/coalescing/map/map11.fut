-- ==
-- input { [1,2,3,4] } auto output
-- structure gpu { SegMap 2 }

def main (xs: []i32) =
  loop xs for _i < 10 do
    map (+ 2) (opaque (map (+ 1) (reverse xs)))
