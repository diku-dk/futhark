-- Type inference should not eliminate uniqueness checking.
-- ==
-- error: Consuming.*"xs"

def f {xs = xs: []i32} : {xs: []i32} = {xs}

def main xs =
  let {xs = ys} = f {xs}
  in ys with [0] = 0
