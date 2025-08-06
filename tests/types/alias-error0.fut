-- No circular types!
--
-- ==
-- error: Unknown type

type t = t

def main (x: t) : t = x
