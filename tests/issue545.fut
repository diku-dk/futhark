-- ==
-- error: consumption

def update (xs: *[]i32) (x: i32) : *[]i32 =
  xs with [0] = x

def apply (f: i32 -> []i32) (x: i32) : []i32 =
  f x

def main (xs: *[]i32) =
  apply (update xs) 2
