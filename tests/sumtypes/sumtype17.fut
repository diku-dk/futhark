-- Lifted type parameters
-- ==
-- input { }
-- output { 1 }

type^ foobar 't '^s = #foo t | #bar s

def main : i32 =
  match (#bar (+ 1)) : foobar i32 (i32 -> i32)
  case (#foo x) -> x
  case (#bar f) -> f 0
