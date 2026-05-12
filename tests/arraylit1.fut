-- Array literals inside parallel section and with an in-place update.
-- ==
-- input { 3 } output { [[1,0,0],[1,1,0],[1,2,0]] }

def main (x: i32) =
  map (\y -> [1, 0, 0] with [1] = y) (0..<x)
