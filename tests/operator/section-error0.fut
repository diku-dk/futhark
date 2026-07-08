-- section type error: no such field after index.
-- ==
-- error: field

def main (xs: []i32) =
  (.[0].f) xs
