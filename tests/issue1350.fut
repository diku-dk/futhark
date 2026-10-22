-- ==
-- tags { no_webgpu }

def matmul A B = map (\a -> map (\b -> f64.sum (map2 (*) a b)) (transpose B)) A

def identity n = tabulate_2d n n (\i j -> f64.bool (i == j))

def relatives_to_absolutes [n] (relatives: [][4][4]f64) (parents: [n]i64) : [n][4][4]f64 =
  loop absolutes = replicate n (identity 4)
  for (relative, parent, i) in zip3 relatives parents (iota n) do
    if parent == -1
    then absolutes with [i] = relative
    else absolutes with [i] = copy (absolutes[parent] `matmul` relative)

def euler_angles_to_rotation_matrix (xzy: [3]f64) : [4][4]f64 =
  let tx = xzy[0]
  let ty = xzy[2]
  let tz = xzy[1]
  let costx = f64.cos (tx)
  let sintx = f64.sin (tx)
  let costy = f64.cos (ty)
  let sinty = f64.sin (ty)
  let costz = f64.cos (tz)
  let sintz = f64.sin (tz)
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

def get_posed_relatives (num_bones: i64) (base_relatives: [][][]f64) (pose_params: [][3]f64) =
  let offset = 3
  let f i = matmul base_relatives[i] (euler_angles_to_rotation_matrix pose_params[i + offset])
  in tabulate num_bones f

entry get_skinned_vertex_positions (num_bones: i64)
                                   (base_relatives: [][][]f64)
                                   (parents: []i64)
                                   (pose_params: [][3]f64) =
  let relatives = get_posed_relatives num_bones base_relatives pose_params
  in relatives_to_absolutes relatives parents
