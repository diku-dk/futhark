-- Based on #2335
-- ==
-- input { 0i32 }
-- output { 10i32 10i32 }

def f (x: i32) = i32.min 10 (x + 1)

entry main (start: i32) =
  loop (current, last) = (f start, start)
  while current != last do
    (f current, current)
