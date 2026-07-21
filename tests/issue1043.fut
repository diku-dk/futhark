-- ==
-- error: aliased to "xs"

def f 'a 'b (g: a -> b) (xs: a) =
  g xs

def main (xs: []i32) : *[]i32 =
  (`f` xs) id
