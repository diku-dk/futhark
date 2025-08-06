-- Inferred-polymorphic function instantiated twice.
-- ==
-- input { 2 true } output { true 2 }

def id x = x

def main (x: i32) (y: bool) : (bool, i32) = (id y, id x)
