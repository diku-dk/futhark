-- Test some subtle things about aliasing when abstract types are involved.

def divergence 'real (op: real -> real -> real) (c0: real) =
  let next (c, i) = ((c `op` c), i + 1)
  in next (c0, 0i32)
