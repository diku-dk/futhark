-- Using the same parameter name twice is forbidden, even when one use
-- is as a size parameter.
--
-- ==
-- error: also bound

def f [m] (m: [m]i64) = m
