-- ==
-- input { 3 [1,2,3] } output { 1 }
-- compiled input { 0 [1,2,3] } error: division by zero
-- structure gpu { SegRed/Apply 1 /Apply 1 }

def f (x: i32) (y: i32) = x / y

def g (x: i32) (y: i32) = #[noinline] f x y

def main y = map (\x -> #[noinline] g x y) >-> i32.sum
