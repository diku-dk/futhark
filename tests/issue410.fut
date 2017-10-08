let sgmScanSum [n] (vals:[n]i32) (flags:[n]bool) : [n]i32 =
  let pairs = scan ( \(v1,f1) (v2,f2) ->
                       let f = f1 || f2
                       let v = if f2 then v2 else v1+v2
                       in (v,f) ) (0,false) (zip vals flags)
  let (res,_) = unzip pairs
  in res

let sgmIota [n] (flags:[n]bool) : [n]i32 =
  let iotas = sgmScanSum (replicate n 1) flags
  in map (-1) iotas

type point = (i32,i32)
type line = (point,point)

let main [h][w][n] (grid:*[h][w]i32) (lines:[n]line) (nn: i32) (idxs: []i32) =
  let iotan = iota n
  let nums = map (\i -> iotan[i]) idxs
  let flags = map (\i -> i != 0 && nums[i] != nums[i-1]) (iota nn)
  let (ps1,ps2) = unzip lines
  let (xs1,ys1) = unzip ps1
  let (xs2,ys2) = unzip ps2
  let xs1 = map (\i -> xs1[i]) idxs
  let ys1 = map (\i -> ys1[i]) idxs
  let xs2 = map (\i -> xs2[i]) idxs
  let ys2 = map (\i -> ys2[i]) idxs
  let dirxs = map (\x1 x2 ->
                        if x2 > x1 then 1
                        else if x1 > x2 then -1
                        else 0) xs1 xs2
  let slops = map (\x1 y1 x2 y2 ->
                        if x2 == x1 then
                        if y2 > y1 then f32(1) else f32(-1)
                        else f32(y2-y1) / f32.abs(f32(x2-x1))) xs1 ys1 xs2 ys2
  let iotas = sgmIota flags
  let xs = map (\x1 dirx i ->
                     x1+dirx*i) xs1 dirxs iotas
  let ys = map (\y1 slop i ->
                     y1+i32(slop*f32(i))) ys1 slops iotas
  let is = map (\x y -> w*y+x) xs ys
  let flatgrid = reshape (h*w) grid
  in scatter flatgrid is (replicate nn 1)
