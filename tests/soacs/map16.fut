-- Map returning an array predicated on the index variable.
--
-- ==
-- input { 2i64 }
-- output { [[0], [1]] }

def main (chunk: i64) : [][]i32 =
  map (\(k: i32) : [1]i32 ->
         if k == 0 then [0] else [1])
      (map i32.i64 (iota (chunk)))
