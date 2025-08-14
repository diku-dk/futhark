def euclid_dist_2 [d] (pt1: [d]f32) (pt2: [d]f32) : f32 =
  f32.sum (map (\x -> x * x) (map2 (-) pt1 pt2))

def cost [n] [k] [d] (points: [n][d]f32) (centres: [k][d]f32) =
  points
  |> map (\p -> map (euclid_dist_2 p) centres)
  |> map f32.minimum
  |> f32.sum

def grad f x = vjp f x 1f32

def main [n] [d] cluster_centres (points: [n][d]f32) =
  grad (cost points) cluster_centres
