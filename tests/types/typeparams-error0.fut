-- Missing parameter to a parametric type.
-- ==
-- error: vector

type vector 't = []t

def main (n: i32) : vector = iota n
