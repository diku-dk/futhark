-- Enums as function arguments.
-- ==
-- input { }
-- output { 2 }

def f (x : #foo | #bar) : i32 =
  match x
    case #foo   -> 1
    case #bar   -> 2

def main : i32 = f #bar
