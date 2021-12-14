-- Careful not to require that the two applications return the same size.
-- ==
-- input { 1i64 2i64 }
-- output { [0i64, 1i64] [0i64] }

def apply2 '^a '^b (f: a -> b) (x: a) (y: a) =
  let a = f x
  let b = f y
  in (b, a)

def main n m =
  apply2 iota n m
