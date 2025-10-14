-- You can't override ||.
-- ==
-- error: \|\|

def (||) (x: bool) (y: bool) = x

def main (x: bool) = x || x
