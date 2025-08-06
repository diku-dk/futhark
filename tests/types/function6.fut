-- A polymorphic function can be used curried.
-- ==
-- input { [1,2,3] } output { [1,2,3] }

def id 't (x: t) = x

def main (xs: []i32) = map id xs
