-- Using the same parameter name twice is forbidden.
--
-- ==
-- error: also bound

def main (x: i32) (x: i32) : i32 = x
