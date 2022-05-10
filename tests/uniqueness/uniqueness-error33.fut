-- Type ascriptions must respect uniqueness.
-- ==
-- error: aliased to "x"

def main (x: []i32) : *[]i32 = x : *[]i32
