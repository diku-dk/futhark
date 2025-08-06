-- Make sure inner shape names are available, even if they are
-- "shadowed" by an outer type ascription.
--
-- ==
-- input { 2 [1i64,2i64,3i64] }
-- output { 4i64 }

def f [n] ((_, elems: [n]i64): (i32, []i64)) : i64 =
  n + elems[0]

def main (x: i32) (y: []i64) : i64 =
  f (x, y)
