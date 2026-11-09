-- ==
-- random input { [10][10][10]i32 } auto output
-- structure gpu { /SegMap 1 /SegMap/Loop 1 }

def main = map (\xss -> #[flattening(only_outer)] map i32.sum xss)
