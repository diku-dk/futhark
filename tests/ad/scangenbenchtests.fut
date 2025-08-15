-- ==
-- tags { autodiff }

-- ==
-- entry: testmm2by2
-- compiled random input { [250][4]i32 } output { true }

-- ==
-- entry: testmm3by3
-- compiled random input { [111][9]i32 } output { true }

-- ==
-- entry: testmm4by4
-- compiled random input { [62][16]i32 } output { true }

-- ==
-- entry: testlin
-- compiled random input { [500][2]i32 } output { true }

-- ==
-- entry: testlin2by2
-- compiled random input { [166][6]i32 } output { true }
def mm2by2 (a1: i32, b1: i32, c1: i32, d1: i32)
           (a2: i32, b2: i32, c2: i32, d2: i32) =
  ( a1 * a2 + b1 * c2
  , a1 * b2 + b1 * d2
  , c1 * a2 + d1 * c2
  , c1 * b2 + d1 * d2
  )

def primal2 [n] (xs: [n](i32, i32, i32, i32)) =
  scan mm2by2 (1, 0, 0, 1) xs

def fromarrs2 = map (\(x: [4]i32) -> (x[0], x[1], x[2], x[3]))
def toarrs2 = map (\(a, b, c, d) -> [a, b, c, d])

def onehot_2d n m x y =
  tabulate_2d n m (\i j -> i32.bool ((i, j) == (x, y)))

def fwd_J2 [n] (input: [n][4]i32) : [n][4][n][4]i32 =
  let input = fromarrs2 input
  in tabulate (n * 4) (\i -> jvp primal2 input (fromarrs2 (onehot_2d n 4 (i / 4) (i % 4))))
     |> map toarrs2
     |> transpose
     |> map transpose
     |> map (map unflatten)

def rev_J2 [n] (input: [n][4]i32) : [n][4][n][4]i32 =
  let input = fromarrs2 input
  in tabulate (n * 4) (\i -> vjp primal2 input (fromarrs2 (onehot_2d n 4 (i / 4) (i % 4))))
     |> unflatten
     |> map (map toarrs2)

entry testmm2by2 [n] (input: [n][4]i32) =
  let fwd = fwd_J2 input
  let rev = rev_J2 input
  in map2 (map2 (map2 (==))) rev fwd |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true

def mm3by3 (a1: i32, b1: i32, c1: i32, d1: i32, e1: i32, f1: i32, g1: i32, h1: i32, i1: i32)
           (a2: i32, b2: i32, c2: i32, d2: i32, e2: i32, f2: i32, g2: i32, h2: i32, i2: i32) =
  ( a1 * a2 + b1 * d2 + c1 * g2
  , a1 * b2 + b1 * e2 + c1 * h2
  , a1 * c2 + b1 * f2 + c1 * i2
  , d1 * a2 + e1 * d2 + f1 * g2
  , d1 * b2 + e1 * e2 + f1 * h2
  , d1 * c2 + e1 * f2 + f1 * i2
  , g1 * a2 + h1 * d2 + i1 * g2
  , g1 * b2 + h1 * e2 + i1 * h2
  , g1 * c2 + h1 * f2 + i1 * i2
  )

def primal3 [n] (xs: [n](i32, i32, i32, i32, i32, i32, i32, i32, i32)) =
  scan mm3by3 (1, 0, 0, 0, 1, 0, 0, 0, 1) xs

def fromarrs3 = map (\(x: [9]i32) -> (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8]))
def toarrs3 = map (\(a, b, c, d, e, f, g, h, i) -> [a, b, c, d, e, f, g, h, i])

def fwd_J3 [n] (input: [n][9]i32) : [n][9][n][9]i32 =
  let input = fromarrs3 input
  in tabulate (n * 9) (\i -> jvp primal3 input (fromarrs3 (onehot_2d n 9 (i / 9) (i % 9))))
     |> map toarrs3
     |> transpose
     |> map transpose
     |> map (map unflatten)

def rev_J3 [n] (input: [n][9]i32) : [n][9][n][9]i32 =
  let input = fromarrs3 input
  in tabulate (n * 9) (\i -> vjp primal3 input (fromarrs3 (onehot_2d n 9 (i / 9) (i % 9))))
     |> unflatten
     |> map (map toarrs3)

entry testmm3by3 [n] (input: [n][9]i32) =
  let fwd = fwd_J3 input
  let rev = rev_J3 input
  in map2 (map2 (map2 (==))) rev fwd |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true

def mm4by4 (a0, b0, c0, d0, e0, f0, g0, h0, i0, j0, k0, l0, m0, n0, o0, p0)
           (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1: i32) =
  ( a0 * a1 + b0 * e1 + c0 * i1 + d0 * m1
  , a0 * b1 + b0 * f1 + c0 * j1 + d0 * n1
  , a0 * c1 + b0 * g1 + c0 * k1 + d0 * o1
  , a0 * d1 + b0 * h1 + c0 * l1 + d0 * p1
  , e0 * a1 + f0 * e1 + g0 * i1 + h0 * m1
  , e0 * b1 + f0 * f1 + g0 * j1 + h0 * n1
  , e0 * c1 + f0 * g1 + g0 * k1 + h0 * o1
  , e0 * d1 + f0 * h1 + g0 * l1 + h0 * p1
  , i0 * a1 + j0 * e1 + k0 * i1 + l0 * m1
  , i0 * b1 + j0 * f1 + k0 * j1 + l0 * n1
  , i0 * c1 + j0 * g1 + k0 * k1 + l0 * o1
  , i0 * d1 + j0 * h1 + k0 * l1 + l0 * p1
  , m0 * a1 + n0 * e1 + o0 * i1 + p0 * m1
  , m0 * b1 + n0 * f1 + o0 * j1 + p0 * n1
  , m0 * c1 + n0 * g1 + o0 * k1 + p0 * o1
  , m0 * d1 + n0 * h1 + o0 * l1 + p0 * p1
  )

def primal4 [n] (xs: [n](i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32)) =
  scan mm4by4 (1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1) xs

def fromarrs4 = map (\(x: [16]i32) -> (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15]))
def toarrs4 = map (\(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) -> [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p])

def fwd_J4 [n] (input: [n][16]i32) : [n][16][n][16]i32 =
  let input = fromarrs4 input
  in tabulate (n * 16) (\i -> jvp primal4 input (fromarrs4 (onehot_2d n 16 (i / 16) (i % 16))))
     |> map toarrs4
     |> transpose
     |> map transpose
     |> map (map unflatten)

def rev_J4 [n] (input: [n][16]i32) : [n][16][n][16]i32 =
  let input = fromarrs4 input
  in tabulate (n * 16) (\i -> vjp primal4 input (fromarrs4 (onehot_2d n 16 (i / 16) (i % 16))))
     |> unflatten
     |> map (map toarrs4)

entry testmm4by4 [n] (input: [n][16]i32) =
  let fwd = fwd_J4 input
  let rev = rev_J4 input
  in map2 (map2 (map2 (==))) rev fwd |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true

def primallin [n] (xs: [n](i32, i32)) =
  scan (\(a1, b1) (a2, b2) -> (a2 + b2 * a1, b1 * b2)) (0, 1) xs

def fromarrslin = map (\x -> (x[0], x[1]))
def toarrslin = map (\(a, b) -> [a, b])

def fwd_Jlin [n] (input: [n][2]i32) =
  let input = fromarrslin input
  in tabulate (n * 2) (\i -> jvp primallin input (fromarrslin (onehot_2d n 2 (i / 2) (i % 2))))
     |> map toarrslin
     |> transpose
     |> map transpose
     |> map (map unflatten)

def rev_Jlin [n] (input: [n][2]i32) =
  let input = fromarrslin input
  in tabulate (n * 2) (\i -> vjp primallin input (fromarrslin (onehot_2d n 2 (i / 2) (i % 2))))
     |> unflatten
     |> map (map toarrslin)

entry testlin [n] (input: [n][2]i32) =
  let fwd = fwd_Jlin input
  let rev = rev_Jlin input
  in map2 (map2 (map2 (==))) rev fwd |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true

def mv2 (a, b, c, d) (e, f) : (i32, i32) =
  ( a * e + b * f
  , c * e + d * f
  )

def vv2 (a, b) (c, d) : (i32, i32) =
  ( a + c
  , b + d
  )

def lino2by2 (d1, c1) (d2, c2) : ((i32, i32), (i32, i32, i32, i32)) =
  (vv2 d2 (mv2 c2 d1), mm2by2 c2 c1)

def primallin2 [n] (as: [n]((i32, i32), (i32, i32, i32, i32))) =
  scan lino2by2 ((0, 0), (1, 0, 0, 1)) as

def fromarrslin2 = map (\x -> ((x[0], x[1]), (x[2], x[3], x[4], x[5])))
def toarrslin2 = map (\((a, b), (c, d, e, f)) -> [a, b, c, d, e, f])

def fwd_Jlin2 [n] (input: [n][6]i32) : [n][6][n][6]i32 =
  let input = fromarrslin2 input
  in tabulate (n * 6) (\i -> jvp primallin2 input (fromarrslin2 (onehot_2d n 6 (i / 6) (i % 6))))
     |> map toarrslin2
     |> transpose
     |> map transpose
     |> map (map unflatten)

def rev_Jlin2 [n] (input: [n][6]i32) : [n][6][n][6]i32 =
  let input = fromarrslin2 input
  in tabulate (n * 6) (\i -> vjp primallin2 input (fromarrslin2 (onehot_2d n 6 (i / 6) (i % 6))))
     |> unflatten
     |> map (map toarrslin2)

entry testlin2by2 [n] (input: [n][6]i32) =
  let fwd = fwd_Jlin2 input
  let rev = rev_Jlin2 input
  in map2 (map2 (map2 (==))) rev fwd |> map (map (reduce (&&) true)) |> map (reduce (&&) true) |> reduce (&&) true
