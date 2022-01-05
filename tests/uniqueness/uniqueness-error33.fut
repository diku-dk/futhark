-- Type ascriptions must respect uniqueness.
-- ==
-- error: \*\[.*\]i32

def main (x: []i32) = x : *[]i32
