-- Consumption of loops with more certain patterns was not tracked
-- correctly.

let main (n: i32) (x: i32) =
  loop a = replicate n x for i < 10 do
    (loop (a) for j < i do a with [j] = 1)
