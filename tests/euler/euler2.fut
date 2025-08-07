-- By considering the terms in the Fibonacci sequence whose values do
-- not exceed four million, find the sum of the even-valued terms.
--
-- ==
-- input { 4000000 }
-- output { 4613732 }

-- One approach: sequentially construct an array containing the
-- Fibonacci numbers, then filter and sum.  If we knew the number of
-- Fibonacci numbers we needed to generate (or even just an upper
-- bound), we could do it in parallel.
--
-- Our approach: simple sequential counting loop.

def main (bound: i32) : i32 =
  let (sum, _, _) =
    loop (sum, fib0, fib1) = (0, 1, 1)
    while fib1 < bound do
      let newsum =
        if fib1 % 2 == 0
        then sum + fib1
        else sum
      in (newsum, fib1, fib0 + fib1)
  in sum
