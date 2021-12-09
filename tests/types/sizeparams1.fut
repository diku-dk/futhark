-- Size-parameterised type in parameter.
-- ==
-- input { empty([0]i32) } output { 0i64 }
-- input { [1,2,3] } output { 3i64 }

type ints [n] = [n]i32

def main [n] (_: ints [n]) : i64 = n
