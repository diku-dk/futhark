-- No cheating uniqueness with tuple shenanigans.
-- ==
-- error: aliased

def main (x: (*[]i32, *[]i32)) : (*[]i32, *[]i32) =
  (x.0, x.0)
