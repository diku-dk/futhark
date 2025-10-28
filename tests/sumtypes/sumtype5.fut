-- Arrays of sumtypes.
-- ==
-- input { }
-- output { [1, -2, 3, -4] }

type foobar = #foo i32 | #bar i32

def f (x: foobar) : i32 =
  match x
  case (#foo y) -> y
  case (#bar y) -> -y

def main : []i32 = map f ([#foo 1, #bar 2, #foo 3, #bar 4] : []foobar)
