-- Type inference should not eliminate uniqueness checking.
-- ==
-- error: Would consume variable "xs"

let f {xs: []i32} : {xs: []i32} = {xs}

let main xs =
  let {xs=ys} = f {xs}
  in ys with [0] = 0
