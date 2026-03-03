-- section type error: slicing a non-array field.
-- ==
-- error: Cannot apply function

def main (x: i32) =
  let r = {f = x}
  in (.f.[0:1]) r
