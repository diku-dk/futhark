-- Mapping a rotation is turned into a single rotate.
-- ==
-- input { [[1,2,3,3],[4,5,6,6],[7,8,9,9]] }
-- output { [[2,3,3,1],[5,6,6,4],[8,9,9,7]] }
-- structure { Map 0 Rotate 1 }

let main (xss: [][]i32): [][]i32 = map (\xs -> rotate 1 xs) xss
