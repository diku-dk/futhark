-- ==
-- input { [[1,2],[4,5],[7,8]] }
-- output { [[1i32, 4i32, 7i32], [2i32, 5i32, 8i32]] }

def main [n] [m] (xss: [n][m]i32) =
  transpose xss :> [2][3]i32
