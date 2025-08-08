-- ==
-- error: aliased to "xs"

def f 'a 'b (f: a -> b) (xs: a) =
  f xs

def main (xs: []i32) : *[]i32 =
  (`f` xs) id
