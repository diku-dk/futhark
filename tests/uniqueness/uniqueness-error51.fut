-- Type inference should not eliminate uniqueness checking.
-- ==
-- error: Would consume variable "xs"

def f {xs: []i32} : {xs: []i32} = {xs}

def main xs =
  let {xs=ys} = f {xs}
  in ys with [0] = 0
