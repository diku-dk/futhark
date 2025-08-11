--
-- ==
-- error: field

def main (x: (i32, i8, i16)) : (i8, i16, i32) =
  (x.0, x.2, x.0)
