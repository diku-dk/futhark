-- Specific number of SegMaps in this test is not so important - we
-- just shouldn't get rid of the versions.
-- ==
-- structure gpu-mem { If/True/SegMap 3 If/False/If/True/SegMap 2 If/False/If/False/If/True/SegMap 1 }

module type rand = {
  type rng
  val init : i32 -> rng
  val rand : rng -> (rng, i32)
  val split : rng -> (rng, rng)
  val split_n : (n: i64) -> rng -> [n]rng
}

-- random module taken from Futharks webpage
module lcg : rand = {
  type rng = u32

  def addConst : u32 = 1103515245
  def multConst : u32 = 12345
  def modConst : u32 = 1 << 31

  def rand rng =
    let rng' = (addConst * rng + multConst) % modConst
    in (rng', i32.u32 rng')

  def init (x: i32) : u32 =
    let x = u32.i32 x
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x) * 0x45d9f3b
    let x = ((x >> 16) ^ x)
    in x

  def split (rng: rng) =
    ( init (i32.u32 (rand rng).0)
    , init (i32.u32 rng)
    )

  def split_n n rng =
    tabulate n (\i -> init (i32.u32 rng ^ i32.i64 i))
}

-- This function swaps the two edges that produce the lowest cost
def swap [m] (i: i32) (j: i32) (tour: [m]i32) : [m]i32 =
  let minI = i + 1
  in map i32.i64 (iota m)
     |> map (\ind ->
               if ind < minI || ind > j
               then tour[ind]
               else tour[j - (ind - minI)])

def rand_nonZero (rng: lcg.rng) (bound: i32) =
  let (rng, x) = lcg.rand rng
  in if (x % bound) > 0 then (rng, x % bound) else (rng, (x % bound) + 1)

def mkRandomTour (offset: i64) (cities: i32) : []i32 =
  let rng = lcg.init (i32.i64 offset)
  --let randIndArr = map (\i ->
  --    if i == 0 then rand_i32 rng cities
  --              else rand_i32 (i-1).0 cities ) iota cities
  let initTour =
    map (\i ->
           if i == (i64.i32 cities)
           then 0
           else i)
        (iota ((i64.i32 cities) + 1))
    |> map i32.i64
  let intialI = rand_nonZero rng (cities)
  let intialJ = rand_nonZero intialI.0 (cities)
  let randomSwaps = rand_nonZero intialJ.0 100
  let rs =
    loop (intialI, intialJ, initTour) for i < randomSwaps.1 do
      let intI = rand_nonZero intialI.0 (cities)
      let intJ = rand_nonZero intI.0 (cities)
      let swappedTour = swap intialI.1 intialJ.1 initTour
      in (intI, intJ, swappedTour)
  in rs.2

-- mkFlagArray is taken from PMPH lecture notes p. 48
def mkFlagArray 't [m]
                (aoa_shp: [m]i32)
                (zero: t)
                (aoa_val: [m]t) : []t =
  let shp_rot =
    map (\i ->
           if i == 0
           then 0
           else aoa_shp[i - 1])
        (iota m)
  let shp_scn = scan (+) 0 shp_rot
  let aoa_len = shp_scn[m - 1] + aoa_shp[m - 1] |> i64.i32
  let shp_ind =
    map2 (\shp ind ->
            if shp == 0
            then -1
            else ind)
         aoa_shp
         shp_scn
  in scatter (replicate aoa_len zero) (map i64.i32 shp_ind) aoa_val

-- segmented_scan is taken from PMPH Futhark code
def segmented_scan [n] 't
                   (op: t -> t -> t)
                   (ne: t)
                   (flags: [n]bool)
                   (arr: [n]t) : [n]t =
  let (_, res) =
    unzip
    <| scan (\(x_flag, x) (y_flag, y) ->
               let fl = x_flag || y_flag
               let vl = if y_flag then y else op x y
               in (fl, vl))
            (false, ne)
            (zip flags arr)
  in res

-- Comparator function used in forLoops in the reduce part
def changeComparator (t1: (i32, i32, i32)) (t2: (i32, i32, i32)) : (i32, i32, i32) =
  if t1.0 < t2.0
  then t1
  else if t1.0 == t2.0
  then if t1.1 < t2.1
       then t1
       else if t1.1 == t2.1
       then if t1.2 < t2.2
            then t1
            else t2
       else t2
  else t2

-- finds the best cost of two input costs
def costComparator (cost1: i32) (cost2: (i32)) : i32 =
  if cost1 < cost2 then cost1 else cost2

-- findMinChange is the parallel implementation of the two for loops
-- in the 2-opt move algorithm
def findMinChange [m] [n] [x] [y]
                  (dist: [m]i32)
                  (tour: [n]i32)
                  (Iarr: [x]i32)
                  (Jarr: [y]i32)
                  (cities: i32)
                  (totIter: i64) : (i32, i32, i32) =
  let changeArr =
    map (\ind ->
           let i = Iarr[ind]
           let iCity = tour[i]
           let iCityp1 = tour[i + 1]
           let j = Jarr[ind] + i + 2
           let jCity = tour[j]
           let jCityp1 = tour[j + 1]
           in ( (dist[iCity * cities + jCity]
                 + dist[iCityp1 * cities + jCityp1]
                 - (dist[iCity * cities + iCityp1]
                    + dist[jCity * cities + jCityp1]))
              , i
              , j
              ))
        (iota totIter)
  in reduce changeComparator (2147483647, -1, -1) changeArr

-- 2-opt algorithm
def twoOptAlg [m] [n] [x] [y]
              (distM: [m]i32)
              (tour: [n]i32)
              (Iarr: [x]i32)
              (Jarr: [y]i32)
              (cities: i32)
              (totIter: i64) : []i32 =
  let twoOpt xs =
    let minChange = findMinChange distM xs Iarr Jarr cities totIter
    in if minChange.0 < 0
       then (swap minChange.1 minChange.2 xs, minChange.0)
       else (xs, minChange.0)
  let rs = loop (xs, cond) = (tour, -1) while cond < 0 do twoOpt xs
  in rs.0

-- Compute cost of tour
def cost [n] [m] (tour: [n]i32) (distM: [m]i32) : i32 =
  map (\i -> distM[tour[i] * i32.i64 (n - 1) + tour[i + 1]])
      (iota (n - 1))
  |> reduce (+) 0

-- Generate flagArray
def flagArrayGen (cities: i32) : ([]i32) =
  let len = i64.i32 (cities - 2)
  let aoa_val = replicate len 1i32
  let temp = map (+ 1) (iota len) |> map i32.i64
  let aoa_shp =
    map (\i ->
           temp[len - i - 1])
        (iota len)
  in mkFlagArray aoa_shp 0i32 aoa_val

--[m]
def main [m]
         (cities: i32)
         (numRestarts: i64)
         (distM: [m]i32) : i32 =
  --let cities = 5
  let totIter = ((cities - 1) * (cities - 2)) / 2 |> i64.i32
  --let initTour = (iota (i64.i32 cities+1)) |>
  --map(\i -> (i+1)*10)

  --let oldCost = cost tour distM
  --let distM = [0,4,6,8,3,
  --             4,0,4,5,2,
  --             6,4,0,2,3,
  --             8,5,2,0,4,
  --             3,2,3,4,0]

  let flagArr = flagArrayGen cities
  let Iarr = scan (+) 0i32 flagArr |> map (\x -> x - 1)
  let Jarr = segmented_scan (+) 0i32 (map bool.i32 (flagArr :> [totIter]i32)) (replicate totIter 1i32) |> map (\x -> x - 1)
  let allCosts =
    map (\ind ->
           let tour = mkRandomTour ((ind + 1) * 13) cities
           let minTour = twoOptAlg distM tour Iarr Jarr cities totIter
           in cost minTour distM)
        (iota numRestarts)
  in reduce costComparator 2147483647 allCosts

--in mkRandomTour 125 cities
