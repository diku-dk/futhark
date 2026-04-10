-- Type error must state field name.
-- ==
-- error: record field "f"

def f (r: {f: i32}) = r with f = true
