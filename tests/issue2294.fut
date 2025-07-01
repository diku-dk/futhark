def pointToSegment (pose: [3]f32) (p: [3]f32) (segment: [2][2]f32) : f32 =
  let cx = pose[0]
  let cy = pose[1]
  let cz = pose[2]
  let p_loc = p |> map2 (+) [-cx, -cy, -cz]
  let t = map2 (+) [p_loc[0], p_loc[2]] <| map2 (+) segment[0] segment[1]
  in f32.sum t

def pointsToSegmentsWeighted [n] [m]
                             (pose: [3]f32)
                             (segments: [m][2][2]f32)
                             (pcd: [n][3]f32) : [n]f32 =
  let computeDistances p = map (pointToSegment pose p) segments
  let byDistance d1 d2 = if d1 <= d2 then d1 else d2
  let selectClosest vs = reduce_comm byDistance f32.highest vs
  in map (computeDistances >-> selectClosest) pcd

entry pointsToSegmentsGrad [n] [m]
                           (pose: [3]f32)
                           (segments: [m][2][2]f32)
                           (pcd: [n][3]f32) : [3 + 1]f32 =
  let go p = f32.sum (pointsToSegmentsWeighted p segments pcd)
  let (loss, grad) = vjp2 go pose 1
  in (grad ++ [loss])

-- ==
-- input { [[1f32,2,3],[4f32,5,6]] }
-- output { [-2.0f32, 0.0, -2.0, 22.0] }
entry main pcd = pointsToSegmentsGrad [0.0, 0, 0] [[[0.0, 0.0], [2.0, 2.0]]] pcd
