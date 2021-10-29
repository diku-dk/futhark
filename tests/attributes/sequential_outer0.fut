-- ==
-- random input { [10][10]i32 } auto output
-- structure gpu { /DoLoop 1 /DoLoop/SegRed 1 }

let main xss = #[sequential_outer] map i32.sum xss
