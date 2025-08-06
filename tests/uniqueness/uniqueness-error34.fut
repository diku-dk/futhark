-- Pattern bindings must respect uniqueness.
-- ==
-- error: aliased to "x"

def main (x: []i32) : *[]i32 = let y: *[]i32 = x in y
