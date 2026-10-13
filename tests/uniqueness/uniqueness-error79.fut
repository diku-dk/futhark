-- A conditional must not forget that two components of its result are the same
-- array, even when one branch consumes the names that recorded it.
-- ==
-- error: consumed

def main (c: bool) (n: i64) : ([]i32, []i32) =
  let u = replicate n 0i32
  let p = (u, u)
  let a = replicate n 1i32
  let b = replicate n 2i32
  let (r0, r1) = if c then p else (let _z = p.0 with [0] = 5 in (a, b))
  let r0[0] = 10
  let r1[0] = 20
  in (r0, r1)
