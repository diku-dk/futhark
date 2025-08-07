-- A reduction whose operator could cause existential memory.
-- ==
-- random input { [40][4]i32 } auto output

def main [n] (xs: [n][4]i32) =
  let op (x: [4]i32) (y: [4]i32): [4]i32 = if x[0] < y[0] then x else y
  in reduce op [i32.lowest, 0, 0, 0] xs
