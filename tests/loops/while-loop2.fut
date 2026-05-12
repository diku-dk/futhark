-- While-loop with a condition that consumes something that it has allocated itself.
-- ==
-- input {
--   [5i64,4i64,2i64,8i64,1i64,9i64,9i64]
--   4i64
-- }
-- output {
--   [5i64, 4i64, 2i64, 8i64, 6i64, 9i64, 9i64]
-- }

def pointlessly_consume (x: i64, a: *[]i64) : bool =
  x < reduce (+) 0 a

def main (a: *[]i64) (i: i64) : []i64 =
  loop (a) while pointlessly_consume (a[i], iota (i)) do
    let a[i] = a[i] + 1 in a
