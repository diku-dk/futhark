-- Reverse an inner array using indexing.
--
-- ==
-- input { [[1,2],[3,4]] }  output { [[2,1],[4,3]] }

let main(as: [][]i32): [][]i32 = as[:,::-1]
