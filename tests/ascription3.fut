-- Array type ascription cannot change the rank of an array.
--
-- ==
-- error: Expression does not have expected type

def main [n] [m] (x: [n][m]i32) = x : []i32
