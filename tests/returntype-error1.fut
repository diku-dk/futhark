-- Test that the subtype property works for function return types.
-- ==
-- error:

def f (a: *[]i32) : []i32 = a

-- OK, because unique is a subtype of nonunique

def g (a: []i32) : *[]i32 = a

-- Wrong!

def main () : i32 = 0
