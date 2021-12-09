-- The simplest conceivable type inference.
-- ==
-- input { 2 } output { 2 }

def id x = x

def main (x: i32) = id x
