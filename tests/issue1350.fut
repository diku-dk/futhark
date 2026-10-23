def matmul A B = map (\a -> map (\b -> f32.sum (map2 (*) a b)) (transpose B)) A

def identity n = tabulate_2d n n (\i j -> f32.bool (i == j))

def relatives_to_absolutes [n] (relatives: [][4][4]f32) (parents: [n]i64) : [n][4][4]f32 =
  loop absolutes = replicate n (identity 4)
  for (relative, parent, i) in zip3 relatives parents (iota n) do
    if parent == -1
    then absolutes with [i] = relative
    else absolutes with [i] = copy (absolutes[parent] `matmul` relative)

def euler_angles_to_rotation_matrix (xzy: [3]f32) : [4][4]f32 =
  let tx = xzy[0]
  let ty = xzy[2]
  let tz = xzy[1]
  let costx = f32.cos (tx)
  let sintx = f32.sin (tx)
  let costy = f32.cos (ty)
  let sinty = f32.sin (ty)
  let costz = f32.cos (tz)
  let sintz = f32.sin (tz)
  in [ [ costy * costz
       , -costx * sintz + sintx * sinty * costz
       , sintx * sintz + costx * sinty * costz
       , 0
       ]
     , [ costy * sintz
       , costx * costz + sintx * sinty * sintz
       , -sintx * costz + costx * sinty * sintz
       , 0
       ]
     , [ -sinty
       , sintx * costy
       , costx * costy
       , 0
       ]
     , [ 0
       , 0
       , 0
       , 1
       ]
     ]

def get_posed_relatives (num_bones: i64) (base_relatives: [][][]f32) (pose_params: [][3]f32) =
  let offset = 3
  let f i = matmul base_relatives[i] (euler_angles_to_rotation_matrix pose_params[i + offset])
  in tabulate num_bones f

entry get_skinned_vertex_positions (num_bones: i64)
                                   (base_relatives: [][][]f32)
                                   (parents: []i64)
                                   (pose_params: [][3]f32) =
  let relatives = get_posed_relatives num_bones base_relatives pose_params
  in relatives_to_absolutes relatives parents
