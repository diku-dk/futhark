-- You can use a constant as a shape declaration in another constant.
--
-- ==
-- input { } output { [0,0,0] }

let n: i32 = 3

let x: [n]i32 = replicate n 0

let main: []i32 = copy x
