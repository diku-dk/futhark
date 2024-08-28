def map_4d 'a 'x [n][m][l][k] (f: a -> x) (as: [n][m][l][k]a): [n][m][l][k]x = map (map (map (map f))) as

def map2_4d 'a 'b 'x [n][m][l][k] (f: a -> b -> x) (as: [n][m][l][k]a) (bs: [n][m][l][k]b): [n][m][l][k]x =
  map2 (map2 (map2 (map2 f))) as bs

-- Bookkeeping
def uoffsets: ([27]i64,[27]i64,[27]i64) =
  ([-1i64, -1i64, -1i64, -1i64, -1i64, -1i64, -1i64, -1i64, -1i64, 0i64, 0i64,
  0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 0i64, 1i64, 1i64, 1i64, 1i64, 1i64, 1i64,
  1i64, 1i64, 1i64], [-1i64, -1i64, -1i64, 0i64, 0i64, 0i64, 1i64, 1i64, 1i64,
  -1i64, -1i64, -1i64, 0i64, 0i64, 0i64, 1i64, 1i64, 1i64, -1i64, -1i64, -1i64,
  0i64, 0i64, 0i64, 1i64, 1i64, 1i64], [-1i64, 0i64, 1i64, -1i64, 0i64, 1i64,
  -1i64, 0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 0i64, 1i64,
  -1i64, 0i64, 1i64, -1i64, 0i64, 1i64, -1i64, 0i64, 1i64])
type index = {x: i64, y: i64, z: i64}

def indexIsInside (nelx :i64, nely :i64, nelz :i64) (idx :index) :bool =
  (idx.x >= 0 && idx.y >= 0 && idx.z >= 0 && idx.x < nelx && idx.y < nely && idx.z < nelz)

def getLocalU [nx][ny][nz] (u :[nx][ny][nz][3]f64) (nodeIndex :index) :*[27*3]f64 =
  let (nodeOffsetsX,nodeOffsetsY,nodeOffsetsZ) = uoffsets
  let ni = nodeIndex
  in (map3 (\i j k ->
    if (indexIsInside (nx,ny,nz) {x=ni.x+i,y=ni.y+j,z=ni.z+k}) then
      #[unsafe] (u[ni.x+i,ni.y+j,ni.z+k])
    else
      [0,0,0]) nodeOffsetsX nodeOffsetsY nodeOffsetsZ)
    |> flatten


-- SOR SWEEP FILE
def omega         :f64 = 0.6
def sorNodeAssembled (mat :[3][27*3]f64) (uStencil :*[27*3]f64) (f :[3]f64) :*[3]f64 =
  -- extract value of own node, and zero
  let ux_old = copy uStencil[39]
  let uy_old = copy uStencil[40]
  let uz_old = copy uStencil[41]

  let uStencil = uStencil with [39] = 0
  let uStencil = uStencil with [40] = 0
  let uStencil = uStencil with [41] = 0
  let S = map (\row -> (f64.sum (map2 (*) uStencil row))) mat
  let M = mat[:3,39:42]

  let rx = M[0,1]*uy_old + M[0,2]*uz_old
  let ux_new = (1/M[0,0]) * (f[0]-S[0]-rx)
  let ry = M[1,0]*ux_new + M[1,2]*uz_old
  let uy_new = (1/M[1,1]) * (f[1]-S[1]-ry)
  let rz = M[2,0]*ux_new + M[2,1]*uy_new
  let uz_new = (1/M[2,2]) * (f[2]-S[2]-rz)

  let uold = [ux_old, uy_old, uz_old]
  let unew = [ux_new, uy_new, uz_new]
  in map2 (\un uo -> omega*un + (1-omega)*uo) unew uold

entry sorSweepAssembled [nx][ny][nz] (mat :[nx][ny][nz][3][27*3]f64) (f :[nx][ny][nz][3]f64) (u :[nx][ny][nz][3]f64) =
  tabulate_3d nx ny nz (\i j k ->
    let uloc = getLocalU u {x=i,y=j,z=k}
    in #[unsafe] sorNodeAssembled mat[i,j,k] uloc f[i,j,k])
