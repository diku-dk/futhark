-- Insufficient polymorphism.
-- ==
-- error: pair

module type has_pair = {val pair 'a 'b : a -> b -> (a, b)}

module with_pair : has_pair = {def pair 'a (x: a) (y: a) = (x, y)}
