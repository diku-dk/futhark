-- Sumtypes as function arguments.
-- ==
-- input { }
-- output { 2 }

type foobar = #foo i32 | #bar i32

def f (x: foobar) : i32 =
  match x
  case (#foo _) -> 1
  case (#bar _) -> 2

def main : i32 = f (#bar 12)
