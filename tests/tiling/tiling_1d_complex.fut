-- More stuff that can go wrong with a larger tiling prelude, but
-- still just 1D tiling.
-- ==
-- no_ispc compiled random input { [2000]f32 [2000]f32 } auto output
-- structure gpu { SegMap/Loop/SegMap 2 }

type point = (f32, f32)

def add_points ((x1, y1): point) ((x2, y2): point) : point =
  (x1 + x2, y1 + y2)

def euclid_dist_2 ((x1, y1): point) ((x2, y2): point) : f32 =
  (x2 - x1) ** 2.0f32 + (y2 - y1) ** 2.0f32

def closest_point (p1: (i32, f32)) (p2: (i32, f32)) : (i32, f32) =
  if p1.1 < p2.1 then p1 else p2

def find_nearest_point [k] (pts: [k]point) (pt: point) : i32 =
  let (i, _) =
    reduce_comm closest_point
                (0, euclid_dist_2 pt pts[0])
                (zip (map i32.i64 (iota k))
                     (map (euclid_dist_2 pt) pts))
  in i

def main [n] (xs: [n]f32) (ys: [n]f32) =
  let points = zip xs ys
  let cluster_centres = take 10 points
  in #[sequential_inner]
     map (find_nearest_point cluster_centres) points
