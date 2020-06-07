-- ==
-- random input { [10][10]i32 } auto output
-- structure distributed { SegMap 0 DoLoop 2 }

let main xss = #[sequential] map i32.sum xss
