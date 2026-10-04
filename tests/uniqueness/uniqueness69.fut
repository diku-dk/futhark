-- A consumed parameter may be returned as fresh.
-- ==
-- input { [1,2] }
-- output { [1,2] }

def main (x: *[]i32) : *[]i32 = x
