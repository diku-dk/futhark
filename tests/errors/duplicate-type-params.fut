-- Using the same type parameter name twice is forbidden.
--
-- ==
-- error: also bound

def f 'a 'a (x: a) = x
