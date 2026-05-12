-- Being more polymorphic is OK.
-- ==
-- input { 1 2 } output { 1 2 }

module type has_pair = {val pair 'a : a -> a -> (a, a)}

module with_pair : has_pair = {def pair 'a 'b (x: a) (y: b) = (x, y)}

def main (x: i32) (y: i32) = with_pair.pair x y
