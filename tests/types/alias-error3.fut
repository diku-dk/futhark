-- You may not define the same alias twice.
--
-- ==
-- error: Duplicate.*mydup

type mydup = i32
type mydup = f32

def main (x: i32) : i32 = x
