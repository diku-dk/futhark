-- How quickly can we reduce arrays?
--
-- ==
-- tags { no_python }
-- input { 0 }
-- output { 0 }
-- input { 100 }
-- output { 4950 }
-- compiled input { 100000 }
-- output { 704982704 }
-- compiled input { 100000000 }
-- output { 887459712 }
-- structure gpu { Iota 0 }

def main (n: i32) : i32 =
  reduce (+) 0 (0..<n)
