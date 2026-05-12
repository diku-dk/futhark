-- Match on a function.
-- ==
-- input { }
-- output { 1 }

def main : i32 =
  match (\x -> x + 1)
  case y -> 1
