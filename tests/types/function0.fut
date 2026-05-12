-- Simplest polymorphic function.
-- ==
-- input { 1 } output { 1 }

def id 't (x: t) : t = x

def main (x: i32) = id x
