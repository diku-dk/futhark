-- ==
-- input { [[[1,2], [3,4]],[[5,6],[7,8]]] }
-- output { [[[1i32, 2i32], [5i32, 6i32]], [[3i32, 4i32], [7i32, 8i32]]] }

let main (matb: [][][]i32) = transpose matb