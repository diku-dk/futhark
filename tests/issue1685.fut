def max (x: i32, y: i32) : i32 =
  if x > y then x else y

def min (x: i32, y: i32) : i32 =
  if x < y then x else y

def mapOp (x: i32) : (i32, i32, i32) =
  (max (x, 0), max (x, 0), x)

def redOp (y: (i32, i32, i32))
          (z: (i32, i32, i32)) : (i32, i32, i32) =
  let (x0, m0, s0) = y
  let (x1, _, s1) = z
  let s2 = s0 + s1
  in ( max (x0, max (x1, s2 - m0))
     , min (s2, m0)
     , s2
     )

entry main (xs: []i32) : i32 =
  let (x, _, _) = reduce redOp (0, 0, 0) (map mapOp xs)
  in x
