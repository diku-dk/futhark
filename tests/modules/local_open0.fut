-- Does local open work at all?
-- ==
-- input { 1 } output { 6 }

module m = {
let x = 2
let y = 3
}

let main(x: i32) = x + m.(x + y)
