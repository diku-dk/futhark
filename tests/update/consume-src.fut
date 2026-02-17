def consume 't (xs: *[]t) : []t = xs

-- Here we must be careful not to duplicate the source expression.
def main (xs: *[](i64, i64)) =
  consume xs with [0].0 = 2
