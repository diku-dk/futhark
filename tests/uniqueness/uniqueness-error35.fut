-- Loop parameters must respect uniqueness.
-- ==
-- error: \*\[.*\]i32

def main (x: []i32) = loop (x: *[]i32) for i < 10 do x
