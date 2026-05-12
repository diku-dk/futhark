-- Futhark has no real strings beyond a little bit of syntactic sugar,
-- so this is the same as reversing an array.
-- ==
-- input { [1,2,3,4] }
-- output { [4,3,2,1] }

def main (s: []i32) = s[::-1]
