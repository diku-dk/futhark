-- Can we overload the minus operator (might conflict with prefix
-- negation).
-- ==
-- input { 2 3 } output { 5 }

fun (x: i32) - (y: i32) = x + y

fun main (x: i32, y: i32) = x - y
