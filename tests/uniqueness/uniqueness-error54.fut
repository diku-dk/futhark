-- ==
-- error: aliased to some other component.

def dup x = (x, x)

def main (xs: []i32) : (*[]i32, *[]i32) =
  dup (copy xs)
