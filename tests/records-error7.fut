-- Specific error message on record field mismatches.
-- ==
-- error: unshared fields: c, d.

def f (v: {a: i32, b: i32, c: i32}) : {a: i32, b: i32, d: i32} = v
