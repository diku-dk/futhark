-- Simple polymorphic higher-order function that takes a function as argument.
-- ==
-- input { 11 true } output { 44 true }
-- input { 7 false } output { 28 false }
def twice 'a (f: a -> a) (x: a): a = f (f x)

def double (x: i32): i32 = x + x

def not (b: bool): bool = notb

def main (x: i32) (b: bool): (i32, bool) = (twice double x, twice not b)