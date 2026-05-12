-- id id id ...
-- ==
-- input { 378 } output { 378 }

def id '^a (x: a) : a = x

def main (x: i32) = id id id id x
