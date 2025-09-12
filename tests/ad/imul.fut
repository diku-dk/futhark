-- Check the absence of integer overflow.
-- ==
-- tags { autodiff }
-- input { 2000000000i32 2000000000i32 } output { -294967296i32 }

def main x y : i32 = vjp (\x -> x * y) x 2
