-- ==
-- input { 3 [1,2,3] } output { [4,5,6] }
-- structure gpu { SegMap/Apply 1 }

let f (x: i32) (y: i32) = x + y

let main y = map (\x -> #[noinline] f x y)
