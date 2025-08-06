-- Lists as payloads
-- ==
-- input { }
-- output { [1, 6] }

-- Note: this test currently fails.

type maybe 'a = #none | #some a

def f (x: maybe ([]i32)) : i32 =
  match x
  case #none -> 1
  case (#some xs) -> foldl (+) 0 xs

def main : []i32 = map f [#none, #some [1, 2, 3]]
