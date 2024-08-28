-- ==
-- input {}
-- output { [100, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9] 11i64 }

def numbers = concat [100] (0..<10i32)
entry main = (numbers, length numbers)
