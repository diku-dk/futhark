-- ==
-- tags { no_opencl no_cuda no_cudatc no_hip no_pyopencl }

let Lmx [nlat] (m:i64) (n:i64) (amm:f32) (cx:[nlat]f32) (x:[nlat]f32) =
  let X = replicate n 0
  let m' = f32.i64 m
  let Sx p = map2 (*) p x |> reduce (+) 0f32
  let p0 = map (\cx -> amm*(1 - cx*cx)**(m'/2)*(-1)**m') cx
  let p1 = map2 (\cx p0 -> cx*p0) cx p0
  let p2 n p1 p0 = map3 (\cx p1 p0 -> cx*p1) cx p1 p0
  let (X, pn, _) = loop (X,p1,p0) for i < (n-m-1) do
                   let pi = p2 (m'+2+f32.i64 i) p1 p0
                   let X[m+2+i] = Sx pi
                   in (X, pi, p1)
  in X

let main (lmax:i64) amm cx gr =
  #[unsafe]
  let f x = tabulate lmax (\m -> Lmx m lmax amm[m] cx x)
  in vjp f cx gr
