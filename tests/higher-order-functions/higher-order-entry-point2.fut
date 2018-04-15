-- A first-order entry point need not be syntactically first-order.
-- ==
-- input { 2 2 } output { 4 }

let plus (x: i32) (y: i32) = x + y

let main = plus
