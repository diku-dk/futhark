-- ==
-- input { [1,2,3] } output { [3,4,5] }
-- structure gpu { SegMap/Apply 1 }

def f (x: i32) = x + 2

def main = map (\x -> #[noinline] f x)
