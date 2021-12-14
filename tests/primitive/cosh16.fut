-- Does the f16.cosh function work?
-- ==
-- input  { [0f16, -1f16, 3.1415927f16, -3.1415927f16] }
-- output { [1.0f16, 1.5430806348152437f16, 11.591951675521519f16, 11.591951675521519f16] }

def main = map f16.cosh
