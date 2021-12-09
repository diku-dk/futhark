-- Do not let ascription screw up uniqueness/aliasing.
-- ==
-- error: Would consume variable "xs"

def f 't (x: t) = id (x : t)
def main (xs: []i32) = f xs with [0] = 0
