-- ==
-- structure { Screma 1 }

let big_sum = i32.sum (iota 1000000)

let main b = if b then big_sum - 1 else big_sum + 1
