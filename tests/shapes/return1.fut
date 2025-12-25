def f (x: i32) = if x > 10 then x else x + 1

entry main (start: i32) =
  loop (current, last) = (f start, start)
  while current != last do
    (f current, current)
