-- Higher-order abstract types may not be array elements!
-- ==
-- error: Cannot create array

module m = {type^ t = i32 -> i32}: {type^ t}

def x : []m.t = []
