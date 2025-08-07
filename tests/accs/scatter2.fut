-- Scattering arrays.
-- ==
-- input { [[0,0,0],[0,0,0],[0,0,0],[0,0,0]] [[1,-1], [0,3]] }
-- output { [[1, 2, 3], [1, 2, 3], [0, 0, 0], [1, 2, 3]] }

import "intrinsics"

def f 't (x: t) (acc: *acc ([]t)) (is: []i32) =
  loop acc for i in is do
    write acc (i64.i32 i) x

def main (xs: *[][]i32) is =
  scatter_stream xs (f [1, 2, 3]) is
