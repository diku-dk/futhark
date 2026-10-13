-- Like dup2d, but one more. The important thing is to detect the uniformity.
-- ==
-- input { [[[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11]], [[12, 13, 14, 15], [16, 17, 18, 19], [20, 21, 22, 23]]] }
-- auto output
-- structure gpu { SegScan 0 SegMap 0 Rearrange 3 }

def dup = replicate 5 >-> transpose >-> flatten

def main (z: [2][3][4]i32) = z |> map (map dup) |> map dup |> dup
