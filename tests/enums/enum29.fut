-- Local ambiguous enum.
-- ==
-- input { }
-- output { [2,2] }

def f (x: #foo | #bar) : [](#foo | #bar) =
  let id 't (x: t): t = x
  in match id x
     case #foo -> [#foo, #foo]
     case #bar -> [#bar, #bar]

def g (x: #foo | #bar) : i32 =
  match x
  case #foo -> 1
  case #bar -> 2

def main : []i32 = map g (f #bar)
