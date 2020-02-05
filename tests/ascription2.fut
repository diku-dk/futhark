-- Array type ascription.
--
-- ==
-- input { [[1,2],[3,4]] 2 2 } output { [[1,2],[3,4]] }
-- input { [[1,2],[3,4]] 1 4 } error: cannot match shape of type.*`\[1\]\[4\]

let main [n][m] (x: [n][m]i32) (a: i32) (b: i32) = x :> [a][b]i32
