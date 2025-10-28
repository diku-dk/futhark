-- Do not evaluate branches unnecessarily...
-- ==
-- input { 0 } output { 0 }

def main (x: i32) =
  match x
  case 0 -> 0
  case _ -> 2 / x
