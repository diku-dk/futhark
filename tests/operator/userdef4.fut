-- Can we define a user-defined operator with prefix notation?
-- ==
-- input { 2 3 } output { -1 }

let (+) (x: i32) (y: i32) = x - y

let main(x: i32, y: i32) = x + y
