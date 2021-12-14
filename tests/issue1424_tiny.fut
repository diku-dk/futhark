-- Small program related to #1424.
-- ==
-- input { 2 [1,2,3] [42] } auto output
-- structure gpu-mem {SegMap 1}

def main n (xs: []i32) (unit: [1]i32) =
  #[sequential_inner]
  map (\x ->
         let arr = replicate 1 x
         in (loop arr for i < n do unit))
      xs
