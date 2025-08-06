-- Access a record field inside a module.
-- ==
-- input { 1 } output { 3 }

module m = {def r = {x = 2}}

def main (x: i32) = m.r.x + x
