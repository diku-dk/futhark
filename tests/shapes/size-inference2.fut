-- Sometimes we should infer that a size cannot be typed.
-- ==
-- error: Dimensions.*do not match

def main [n] (xs: [n]i32) : [n]i32 = iota (length xs)
