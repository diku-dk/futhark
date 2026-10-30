-- Based on issue 1351.
-- ==
-- input { [[1.0f32,2.0f32,3.0f32],[4.0f32,5.0f32,6.0f32]] 0i64 4i64 }

def main (xs: [][]f32) i j = (.[i:j]) <| iota (i + j)
