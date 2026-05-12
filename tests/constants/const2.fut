-- Can value declarations refer to each other?
--
-- ==
-- input { } output { 3 }

def x : i32 = 2
def y : i32 = x + 1

def main : i32 = y
