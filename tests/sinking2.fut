-- Sinking can be safe even in the presence of in-place updates.
-- ==
-- structure gpu { /SegMap/Index 1 }

def main (n: i64) (as: []i32) (bs: []i32) (cs: []i32) (ds: []i32) (es: []i32) =
  map5 (\a b c d e ->
          let arr = loop arr = replicate n 0 for i < n do arr with [i] = a
          in if a != 1337 then arr else replicate n (b + c + d + e))
       as
       bs
       cs
       ds
       es
