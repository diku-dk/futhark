-- Local open with a nested module.
-- ==
-- input { 1 } output { 6 }

module m0 = {
module m1 = {
let x = 2
}
let x = 3
}

let main(x: i32) = x + m0.(x + m1.(x))
