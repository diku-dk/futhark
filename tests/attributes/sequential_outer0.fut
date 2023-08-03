-- ==
-- random input { [10][10]i32 } auto output
-- structure gpu { /Loop 1 /Loop/SegRed 1 }

def main xss = #[sequential_outer] map i32.sum xss
