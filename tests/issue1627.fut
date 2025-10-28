type isoCoordinates = {xi: f64, eta: f64, zeta: f64}
type index = {x: i64, y: i64, z: i64}

def pt : f64 = 1f64 / f64.sqrt (3f64)

def quadpoints : [8]isoCoordinates =
  [ {xi = pt, eta = pt, zeta = pt}
  , {xi = (-pt), eta = pt, zeta = pt}
  , {xi = pt, eta = (-pt), zeta = pt}
  , {xi = (-pt), eta = (-pt), zeta = pt}
  , {xi = pt, eta = pt, zeta = (-pt)}
  , {xi = (-pt), eta = pt, zeta = (-pt)}
  , {xi = pt, eta = (-pt), zeta = (-pt)}
  , {xi = (-pt), eta = (-pt), zeta = (-pt)}
  ]

def matmul_f64 [n] [m] [p] (A: [n][m]f64) (B: [m][p]f64) : [n][p]f64 =
  map (\A_row ->
         map (\B_col ->
                f64.sum (map2 (*) A_row B_col))
             (transpose B))
      A

def Cmat : [6][6]f64 = [[0.35e2 / 0.26e2, 0.15e2 / 0.26e2, 0.15e2 / 0.26e2, 0, 0, 0], [0.15e2 / 0.26e2, 0.35e2 / 0.26e2, 0.15e2 / 0.26e2, 0, 0, 0], [0.15e2 / 0.26e2, 0.15e2 / 0.26e2, 0.35e2 / 0.26e2, 0, 0, 0], [0, 0, 0, 0.5e1 / 0.13e2, 0, 0], [0, 0, 0, 0, 0.5e1 / 0.13e2, 0], [0, 0, 0, 0, 0, 0.5e1 / 0.13e2]]

def getB0 (iso: isoCoordinates) : [6][24]f64 =
  let t1 = 1 - iso.zeta
  let t2 = 1 + iso.zeta
  let t3 = 0.1e1 / 0.4e1 - iso.eta / 4
  let t4 = t3 * t1
  let t5 = t3 * t2
  let t6 = 0.1e1 / 0.4e1 + iso.eta / 4
  let t7 = t6 * t1
  let t8 = t6 * t2
  let t9 = 1 - iso.xi
  let t10 = 1 + iso.xi
  let t11 = t9 / 4
  let t12 = t11 * t2
  let t13 = t10 / 4
  let t14 = t13 * t1
  let t2 = t13 * t2
  let t1 = t11 * t1
  let t11 = t3 * t10
  let t10 = t6 * t10
  let t3 = t3 * t9
  let t6 = t6 * t9
  in [ [-t4, 0, 0, t4, 0, 0, t7, 0, 0, -t7, 0, 0, -t5, 0, 0, t5, 0, 0, t8, 0, 0, -t8, 0, 0]
     , [0, -t1, 0, 0, -t14, 0, 0, t14, 0, 0, t1, 0, 0, -t12, 0, 0, -t2, 0, 0, t2, 0, 0, t12, 0]
     , [0, 0, -t3, 0, 0, -t11, 0, 0, -t10, 0, 0, -t6, 0, 0, t3, 0, 0, t11, 0, 0, t10, 0, 0, t6]
     , [-t1, -t4, 0, -t14, t4, 0, t14, t7, 0, t1, -t7, 0, -t12, -t5, 0, -t2, t5, 0, t2, t8, 0, t12, -t8, 0]
     , [0, -t3, -t1, 0, -t11, -t14, 0, -t10, t14, 0, -t6, t1, 0, t3, -t12, 0, t11, -t2, 0, t10, t2, 0, t6, t12]
     , [-t3, 0, -t4, -t11, 0, t4, -t10, 0, t7, -t6, 0, -t7, t3, 0, -t5, t11, 0, t5, t10, 0, t8, t6, 0, -t8]
     ]

def getQuadraturePointStiffnessMatrix (youngsModule: f64) (iso: isoCoordinates) : [24][24]f64 =
  let B0 = getB0 iso
  let C = map (map (* youngsModule)) (copy Cmat)
  let inter_geom = matmul_f64 (transpose B0) C
  in matmul_f64 inter_geom B0

def assembleElementNonlinearStiffnessMatrix (youngsModule: f64) : [24][24]f64 =
  map (getQuadraturePointStiffnessMatrix youngsModule) quadpoints
  |> transpose
  |> map transpose
  |> map (map f64.sum)

def getElementStiffnessDiagonal [nelx] [nely] [nelz] (x: [nelx][nely][nelz]f32) (elementIndex: index) =
  let xloc = f64.f32 (x[elementIndex.x, elementIndex.y, elementIndex.z])
  let kt = assembleElementNonlinearStiffnessMatrix xloc
  in kt[8, 8]

entry main [nelx] [nely] [nelz] (x: [nelx][nely][nelz]f32) =
  tabulate_3d nelx nely nelz (\i j k -> getElementStiffnessDiagonal x {x = i, y = j, z = k})

-- ==
-- entry: main
-- input { [[[1f32, 1f32]]] } auto output
