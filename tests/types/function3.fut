-- Anonymous array tuple element type.
-- ==
-- input { [1,2,3] [false,true,true] } output { [3,2,1] [true,true,false] }

let reverse [n] 'a 'b (a: [n](a,b)): [n](a,b) = a[::-1]

let main (x: []i32) (y: []bool) = unzip (reverse (zip x y))
