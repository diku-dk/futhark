-- Currying overloaded operators.

let eq1 = (==1)
let eq2 = (==) : (i32 -> i32 -> bool)
let add1 = (+1)
let add2 = (+) : (i32 -> i32 -> i32)

let main (x: i32) = eq1 x && eq2 (add1 x) (add2 x x)
