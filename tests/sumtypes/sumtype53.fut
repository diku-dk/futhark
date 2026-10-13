-- ==
-- input { [1,2,-1,3] }
-- output { 1i64 }

type^ two '~b = #foo b | #bar b

def mk 'a '~b (f: a -> b) (x: a) : two b = #foo (f x)

entry main (l: []i32) =
  match mk (filter (< 0i32)) l
  case #foo a -> length a
  case #bar b -> length b + 100
