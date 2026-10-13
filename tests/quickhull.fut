-- Recursive data-parallel quickhull.
--
-- The structure test is to verify that inlining happens as expected.
-- ==
-- input { [[0.0,0.0],[2.0,0.0],[2.0,2.0],[0.0,2.0],[1.0,1.0],[1.5,0.5],[0.5,1.5]] }
-- output { [[0.0, 0.0], [0.0, 2.0], [2.0, 2.0], [2.0, 0.0]] }
-- structure { Apply 3 }

type dist = f64
type point = {x: f64, y: f64}

def point_eq (p: point) (q: point) =
  p.x == q.x && p.y == q.y

def point_leq (p: point) (q: point) =
  p.x <= q.x || (p.x == q.x && p.y <= q.y)

def point_less (p: point) (q: point) =
  p.x < q.x || (p.x == q.x && p.y < q.y)

def sqr (x: f64) = x * x
def ssqr (x: f64) = f64.abs x * x

def signed_dist_to_line (p: point) (q: point) (r: point) =
  let ax = q.x - p.x
  let ay = q.y - p.y
  let bx = r.x - p.x
  let by = r.y - p.y
  in ssqr (ax * by - ay * bx) / (sqr ax + sqr ay)

def minimum [n] 't ((<=): t -> t -> bool) (a: [n]t) : t =
  reduce (\x y -> if x <= y then x else y) a[0] a

def maximum [n] 't ((<=): t -> t -> bool) (a: [n]t) : t =
  minimum (flip (<=)) a

-- Positive if p is to the left of directed line ab.
def side (a: point) (b: point) (p: point) =
  (b.x - a.x) * (p.y - a.y) - (b.y - a.y) * (p.x - a.x)

def farthest (a: point) (b: point) =
  let dist p = f64.abs (side a b p)
  in maximum (\p q -> dist p <= dist q)

def hull [n] (a: point) (b: point) (pts: [n]point) : []point =
  if n <= 1
  then pts
  else let p = farthest a b pts
       let f i =
         let (x, y, pts') =
           if i == 0
           then (a, p, filter (\q -> side a p q > 0) pts)
           else if i == 1
           then (a, p, [p])
           else (p, b, filter (\q -> side p b q > 0) pts)
         in hull x y pts'
       let (_, _, _, pts') = flatmap' f [0, 1, 2]
       in pts'

def quickhull (pts: []point) : []point =
  let left = minimum point_leq pts
  let right = maximum point_leq pts
  let above = filter (\p -> side left right p > 0) pts
  let below = filter (\p -> side left right p < 0) pts
  in [left] ++ hull left right above ++ [right] ++ hull right left below

entry main xs =
  map (\p -> {x = p[0], y = p[1]}) xs
  |> quickhull
  |> map (\{x, y} -> [x, y])
