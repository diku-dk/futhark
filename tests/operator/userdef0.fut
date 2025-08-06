-- Can we define a user-defined operator at all?
-- ==
-- input { 2 3 } output { -1 }

def (+) (x: i32) (y: i32) = x - y

def main (x: i32) (y: i32) = x + y
