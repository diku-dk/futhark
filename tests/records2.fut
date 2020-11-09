-- Records can be used like tuples.
-- ==
-- input { 2 } output { 3 1 }

let f(x: i32) = {0=x+1,1=x-1}

let main(x: i32) = f x
