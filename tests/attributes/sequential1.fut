-- ==
-- random input { [10][10]i32 } auto output
-- structure gpu { /SegMap 1 /SegMap/DoLoop 1 }

let main xss = map (\xs -> #[sequential] i32.sum xs) xss
