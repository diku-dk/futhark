-- N-ary sumtypes.
-- ==
-- input { }
-- output { 26 }

type^ foobar = #foo i32 | #bar i32
type^ boomoo = #boo foobar i32 {field1: i32, field2: []i32} |  #moo i32 foobar

def main : i32 =
  match ((#boo (#bar 5) 10 {field1 = 1, field2 = [1,2,3,4]}) : boomoo)
  case (#boo (#bar 5) 10 {field1 = 2, field2 = _}) -> 1
  case (#boo (#bar 1) 10 {field1 = 1, field2 = _}) -> 2
  case (#boo (#bar x) y {field1 = w, field2 = v}) -> x + y + w + foldl (+) 0 v
  case _ -> 3
