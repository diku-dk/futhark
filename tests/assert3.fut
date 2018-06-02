-- unsafe can remove assertions.
-- ==
-- input { 2 } output { 1 }
-- input { 3 } output { 1 }

let main (x: i32) = unsafe assert (x%2 == 0) (x/2)
