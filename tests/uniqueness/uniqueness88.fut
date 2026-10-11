-- The observed components of a loop parameter may coincide, even when another
-- component is consumed.
-- ==
-- input { [1, 2, 3] [4, 5, 6] }
-- output { [8, 2, 3] [4, 5, 6] [4, 5, 6] }

def main (x: *[]i32) (y: []i32) =
  loop (a, b, c) = (x, y, y) for i < 1 do
    let a[0] = b[0] + c[0]
    in (a, b, c)
