-- ==
-- input { [7i64, 14i64, 14i64] }
-- auto output
-- structure gpu { /SegMap 1 /SegMap/Loop 1 }

def main (ns: []i64) =
  map (\n ->
         let xs =
           let m = opaque (n * 2)
           in loop x = 5
              for i < 10 do
                let ys = opaque (replicate m (i + x + m))
                in ys[6]
         in xs + n)
      ns
