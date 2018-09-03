-- Can value declarations refer to each other?
--
-- ==
-- input { } output { 3 }

let x: i32 = 2
let y: i32 = x + 1

let main: i32 = y
