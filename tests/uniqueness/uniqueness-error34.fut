-- Pattern bindings must respect uniqueness.
-- ==
-- error: \*\[.*\]i32

def main (x: []i32) = let y : *[]i32 = x in y
