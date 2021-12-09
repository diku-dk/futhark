-- Curried entry point.
-- ==
-- input { 2 2 } output { 4 }

def plus (x: i32) (y: i32) = x + y

def main (x: i32) = plus x
