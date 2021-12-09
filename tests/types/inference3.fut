-- An inferred parameter can be put in an array.
-- ==
-- input { 2 } output { [2] }

def f x = [x]

def main (x: i32) = f x
