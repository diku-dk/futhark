-- ==
-- random input { [10][10]i32 } auto output
-- structure gpu { /SegRed 1 }

def main xsss = #[sequential_inner] map_stream (\k (xss: [k][]i32) -> map i32.sum xss) xsss
