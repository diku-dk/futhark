-- Scan with linear function composition.
-- MatrixMul case
-- ==
-- entry: fwd_J rev_J
-- input { [[1f32, 2f32], [4f32, 3f32], [3f32, 4f32], [4f32, 2f32]] }
-- output {
-- [[[[1f32, 0f32], [0f32, 0f32], [0f32, 0f32], [0f32, 0f32]],
--   [[0f32, 1f32], [0f32, 0f32], [0f32, 0f32], [0f32, 0f32]]],
--  [[[3f32, 0f32], [1f32, 1f32], [0f32, 0f32], [0f32, 0f32]],
--   [[0f32, 3f32], [0f32, 2f32], [0f32, 0f32], [0f32, 0f32]]],
--  [[[12f32, 0f32], [4f32, 4f32], [1f32, 7f32], [0f32, 0f32]],
--   [[0f32, 12f32], [0f32, 8f32], [0f32, 6f32], [0f32, 0f32]]],
--  [[[24f32, 0f32], [8f32, 8f32], [2f32, 14f32], [1f32, 31f32]],
--   [[0f32, 24f32], [0f32, 16f32], [0f32, 12f32], [0f32, 24f32]]]]
-- }

def primal [n] (xs: [n](f32,f32)) =
  scan (\(a1,b1) (a2,b2) -> (a2 + b2*a1, b1*b2)) (0,1) xs

def fromarrs = map (\x -> (x[0],x[1]))
def toarrs = map (\(a,b) -> [a,b])

def onehot_2d n m x y =
  tabulate_2d n m (\i j -> f32.bool((i,j) == (x,y)))

entry fwd_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate (n*2) (\i -> jvp primal input (fromarrs (onehot_2d n 2 (i/2) (i%2))))
     |> map toarrs |> transpose |> map transpose |> map (map unflatten)

entry rev_J [n] (input: [n][2]f32) =
  let input = fromarrs input
  in tabulate (n*2) (\i -> vjp primal input (fromarrs (onehot_2d n 2 (i/2) (i%2))))
     |> unflatten |> map (map toarrs)

-- ==
-- entry: fwd_J2 rev_J2
-- no_oclgrind input { [[1f32,2f32,3f32,4f32,5f32,6f32],[6f32,5f32,4f32,3f32,2f32,1f32],[4f32,5f32,6f32,1f32,2f32,3f32],[3f32,2f32,1f32,6f32,5f32,4f32]] }
-- output { [[[[1f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 1f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 1f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 1f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 1f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 0f32, 0f32, 1f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]], [[[4f32, 3f32, 0f32, 0f32, 0f32, 0f32], [1f32, 0f32, 1f32, 2f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[2f32, 1f32, 0f32, 0f32, 0f32, 0f32], [0f32, 1f32, 0f32, 0f32, 1f32, 2f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 4f32, 0f32, 3f32, 0f32], [0f32, 0f32, 3f32, 5f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 4f32, 0f32, 3f32], [0f32, 0f32, 4f32, 6f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 2f32, 0f32, 1f32, 0f32], [0f32, 0f32, 0f32, 0f32, 3f32, 5f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 2f32, 0f32, 1f32], [0f32, 0f32, 0f32, 0f32, 4f32, 6f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]], [[[26f32, 19f32, 0f32, 0f32, 0f32, 0f32], [6f32, 1f32, 6f32, 12f32, 1f32, 2f32], [1f32, 0f32, 16f32, 9f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[14f32, 9f32, 0f32, 0f32, 0f32, 0f32], [2f32, 3f32, 2f32, 4f32, 3f32, 6f32], [0f32, 1f32, 0f32, 0f32, 16f32, 9f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 26f32, 0f32, 19f32, 0f32], [0f32, 0f32, 18f32, 30f32, 3f32, 5f32], [0f32, 0f32, 27f32, 11f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 26f32, 0f32, 19f32], [0f32, 0f32, 24f32, 36f32, 4f32, 6f32], [0f32, 0f32, 34f32, 14f32, 0f32, 0f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 14f32, 0f32, 9f32, 0f32], [0f32, 0f32, 6f32, 10f32, 9f32, 15f32], [0f32, 0f32, 0f32, 0f32, 27f32, 11f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 14f32, 0f32, 9f32], [0f32, 0f32, 8f32, 12f32, 12f32, 18f32], [0f32, 0f32, 0f32, 0f32, 34f32, 14f32], [0f32, 0f32, 0f32, 0f32, 0f32, 0f32]]], [[[110f32, 73f32, 0f32, 0f32, 0f32, 0f32], [18f32, 19f32, 18f32, 36f32, 19f32, 38f32], [1f32, 6f32, 16f32, 9f32, 96f32, 54f32], [1f32, 0f32, 109f32, 64f32, 0f32, 0f32]], [[186f32, 131f32, 0f32, 0f32, 0f32, 0f32], [38f32, 17f32, 38f32, 76f32, 17f32, 34f32], [5f32, 4f32, 80f32, 45f32, 64f32, 36f32], [0f32, 1f32, 0f32, 0f32, 109f32, 64f32]], [[0f32, 0f32, 110f32, 0f32, 73f32, 0f32], [0f32, 0f32, 54f32, 90f32, 57f32, 95f32], [0f32, 0f32, 27f32, 11f32, 162f32, 66f32], [0f32, 0f32, 173f32, 87f32, 0f32, 0f32]], [[0f32, 0f32, 0f32, 110f32, 0f32, 73f32], [0f32, 0f32, 72f32, 108f32, 76f32, 114f32], [0f32, 0f32, 34f32, 14f32, 204f32, 84f32], [0f32, 0f32, 218f32, 110f32, 0f32, 0f32]], [[0f32, 0f32, 186f32, 0f32, 131f32, 0f32], [0f32, 0f32, 114f32, 190f32, 51f32, 85f32], [0f32, 0f32, 135f32, 55f32, 108f32, 44f32], [0f32, 0f32, 0f32, 0f32, 173f32, 87f32]], [[0f32, 0f32, 0f32, 186f32, 0f32, 131f32], [0f32, 0f32, 152f32, 228f32, 68f32, 102f32], [0f32, 0f32, 170f32, 70f32, 136f32, 56f32], [0f32, 0f32, 0f32, 0f32, 218f32, 110f32]]]] }
def mm2by2  (a1, b1, c1, d1)
            (a2, b2, c2, d2) : (f32,f32,f32,f32) =
  ( a1*a2 + b1*c2
  , a1*b2 + b1*d2
  , c1*a2 + d1*c2
  , c1*b2 + d1*d2
  )

def mv2 (a, b, c, d) (e, f): (f32,f32) =
  ( a*e + b*f
  , c*e + d*f)

def vv2 (a, b) (c, d): (f32,f32) =
  ( a+c
  , b+d)

def lino2by2 (d1,c1) (d2,c2) : ((f32,f32), (f32,f32,f32,f32)) =
  (vv2 d2 (mv2 c2 d1),mm2by2 c2 c1)

def primal2 [n] (as: [n]((f32,f32), (f32,f32,f32,f32))) =
  scan lino2by2 ((0,0),(1,0,0,1)) as

def fromarrs2 = map (\x -> ((x[0],x[1]),(x[2],x[3],x[4],x[5])))
def toarrs2 = map (\((a,b),(c,d,e,f)) -> [a,b,c,d,e,f])

entry fwd_J2 [n] (input: [n][6]f32) : [n][6][n][6]f32 =
  let input = fromarrs2 input
  in tabulate (n*6) (\i -> jvp primal2 input (fromarrs2 (onehot_2d n 6 (i/6) (i%6))))
     |> map toarrs2 |> transpose |> map transpose |> map (map unflatten)

entry rev_J2 [n] (input: [n][6]f32) : [n][6][n][6]f32 =
  let input = fromarrs2 input
  in tabulate (n*6) (\i -> vjp primal2 input (fromarrs2 (onehot_2d n 6 (i/6) (i%6))))
     |> unflatten |> map (map toarrs2)
