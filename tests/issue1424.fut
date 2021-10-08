-- ==
-- tags { no_opencl no_cuda no_pyopencl }

type index       = {x: i64, y: i64, z: i64}

let E    :f64 = 1
let Emin :f64 = 1e-6

let indexIsInside (nelx :i64, nely :i64, nelz :i64) (idx :index) :bool =
  (idx.x >= 0 && idx.y >= 0 && idx.z >= 0 && idx.x < nelx && idx.y < nely && idx.z < nelz)

let isOnBoundary(nodeIndex :index, nx :i64, ny :i64, nz :i64) :bool = (nodeIndex.x == 0)

type nodalWeights = (f64, f64)

-- utility methods for doing averages for the element edge, surface, and center
let sumEdge (a :f64, b :f64)                 = 0.5*(a+b)
let sumSurf (a :f64, b :f64, c :f64, d :f64) = 0.25*(a+b+c+d)
let sumCent (w :nodalWeights)                = 0.125*(w.0+w.1)

let prolongateCell (cellIndex :index, w :nodalWeights) =
  let i = cellIndex
  let c   = sumCent w
  let sxp = sumSurf (w.1,w.1,w.1,w.1)  -- Surface X positive
  let sxn = sumSurf (w.0,w.1,w.1,w.1)
  let syp = sumSurf (w.1,w.0,w.1,w.1)
  let syn = sumSurf (w.1,w.1,w.1,w.1)
  let szp = sumSurf (w.1,w.1,w.1,w.1)
  let szn = sumSurf (w.0,w.1,w.1,w.1)
  in [({x=(2*i.x+0),y=(2*i.y+1),z=(2*i.z+0)}, (w.0, sumEdge(w.0,w.1))),
      ({x=(2*i.x+1),y=(2*i.y+1),z=(2*i.z+0)}, (sumEdge(w.1,w.0), w.1))]

let generateLoad (o :i64) (w :nodalWeights) :[24]f64 =
  scatter (replicate 24 0) [0+o,3+o,6+o,9+o,12+o,15+o,18+o,21+o] [w.0, w.1, w.1, w.1, w.0, w.0, w.0, w.1]

let applyBoundaryConditionsToWeightsInverse (elementIndex :index, w :nodalWeights) (nx :i64, ny :i64, nz :i64) :nodalWeights =
  let ei = elementIndex
  let setIfNotIndex (v :f64) (nodeIndex :index) :f64 = if (isOnBoundary (nodeIndex,nx,ny,nz)) then (v/8) else 0
  in (setIfNotIndex w.0 {x=ei.x+0,y=ei.y+1,z=ei.z+0},
      setIfNotIndex w.1 {x=ei.x+1,y=ei.y+1,z=ei.z+0})

let getFineValue [nelx][nely][nelz] (x :[nelx][nely][nelz]f32) (o :i64) (cellIndex :index, w :nodalWeights) :[24]f64 =
  let w_onBoundary = applyBoundaryConditionsToWeightsInverse (cellIndex, w) ((nelx+1),(nely+1),(nelz+1))
  let loadVectorOnBoundary = generateLoad o w_onBoundary
  in loadVectorOnBoundary

let restrictCell (vals :[8][24]f64) :[24]f64 =
  vals |> transpose |> map (\x -> reduce (+) 0 x)

let getDiagonalCellContribution [nelx][nely][nelz] (l :u8) (x :[nelx][nely][nelz]f32) (cellIndex, w) =
  let fineCells =
    loop vals = [(cellIndex,w)] for i < (i64.u8 l) do
      vals |> map prolongateCell |> flatten_to (8**(i+1))
  let fineValuesX = map (getFineValue x 0) fineCells
  let coarseValues =
    loop vx = fineValuesX for i < (i64.u8 l) do
      let ii = (i64.u8 l) - i - 1
      let xx = vx |> unflatten (8**(ii)) 8 |> map restrictCell
      in xx
  let coarseX = flatten_to 24 coarseValues
  in coarseX

entry getNodeDiagonalValues [nelx][nely][nelz] (l :u8) (x :[nelx][nely][nelz]f32) input =
  map (getDiagonalCellContribution l x) input
