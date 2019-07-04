-- More stuff that can go wrong with a larger tiling prelude, but
-- still just 1D tiling.
-- ==
-- compiled random input { [2000]f32 [2000]f32 } auto output
-- structure distributed { SegMap/DoLoop/SegMap 2 }

type point = (f32,f32)

let add_points ((x1,y1): point) ((x2,y2): point): point =
  (x1+x2, y1+y2)

let euclid_dist_2 ((x1,y1): point) ((x2,y2): point): f32 =
  (x2-x1)**2.0f32 + (y2-y1)**2.0f32

let closest_point (p1: (i32, f32)) (p2: (i32, f32)): (i32, f32) =
  if p1.2 < p2.2 then p1 else p2

let find_nearest_point [k] (pts: [k]point) (pt: point): i32 =
  let (i, _) = reduce_comm closest_point (0, euclid_dist_2 pt pts[0])
                           (zip (0..<k) (map (euclid_dist_2 pt) pts))
  in i

let main [n] (xs: [n]f32) (ys: [n]f32) =
  let points = zip xs ys
  let cluster_centres = take 10 points
  in map (find_nearest_point cluster_centres) points
