-- Use of a variable free in a loop can affect inference.
-- ==
-- input { 3 [1,2,3] } output { 3 }

let main m xs = loop y = 0 for i < m do xs[i]
