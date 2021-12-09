-- An inferred parameter can be returned from a branch.
-- ==
-- input { 2 } output { 2 }

def f x = if true then x else x

def main (x: i32) = f x
