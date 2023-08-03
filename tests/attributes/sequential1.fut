-- ==
-- random input { [10][10]i32 } auto output
-- structure gpu { /SegMap 1 /SegMap/Loop 1 }

def main xss = map (\xs -> #[sequential] i32.sum xs) xss
