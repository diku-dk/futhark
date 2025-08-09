-- Does iota work at all?
-- ==
-- input { 0i64 }
-- output { empty([0]i64) }
-- input { 2i64 }
-- output { [0i64,1i64] }

def main (n: i64) : []i64 = iota (n)
