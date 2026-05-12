-- ==
-- input { [1,2,3] }
-- output { [1,2,3] }

def inc (x: i64) = x + 1

def tail [n] 't (A: [inc n]t) = A[1:] :> [n]t

def cons [n] 't (x: t) (A: [n]t) : [inc n]t = [x] ++ A :> [inc n]t

def main (xs: []i32) = tail (cons 2 xs)
