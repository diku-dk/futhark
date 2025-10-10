def (++) 't [n][m] (xs: [n]t) (ys: [m]t): *[n+m]t =
  tabulate (n+m) (\i -> if i < n then xs[i] else ys[i-n])

def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def to_i64 c : i64 = if c then 1 else 0

def filter_indices [n]
  (cs: [n]bool)
 : {(i64, [n]i64) | \(m, is) ->
     FiltPartInv is (\i -> cs[i]) (\_i -> true)
       && (m == sum (map (\x -> to_i64 x) cs))
   } =
  let num_trues = scan (+) 0 (map (\c -> to_i64 c) cs)
  let new_size = if n > 0 then num_trues[n-1] else 0
  let is = map2 (\c i -> if c then i-1 else -1) cs num_trues
  in (new_size, is)

def partition_indices [n]
  (conds: [n]bool)
 : {(i64, [n]i64) | \(split, inds) ->
     FiltPartInv inds (\_i -> true) (\i -> conds[i])
       && split == sum (map (\c -> if c then 1 else 0) conds)
   } =
  let tflgs = map (\c -> if c then 1 else 0) conds
  let fflgs = map (\ b -> 1 - b) tflgs
  let indsT = scan (+) 0 tflgs
  let tmp   = scan (+) 0 fflgs
  let lst   = if n > 0 then indsT[n-1] else 0
  let indsF = map (\t -> t +lst) tmp
  let inds  = map3 (\ c indT indF -> if c then indT-1 else indF-1) conds indsT indsF
  in  (lst, inds)

def partition3_indices [n] 't (conds: [n]i8) 
  : {(i64,i64,[n]i64) | \(_,_,is) ->
      FiltPartInv2 is (\_i -> true) (\i -> conds[i] == 1) (\i -> conds[i] == 2)
   } =
  let tflags = map (\c -> if c == 1 then 1 else 0 ) conds
  let eflags = map (\c -> if c == 2 then 1 else 0 ) conds

  let indsL = scan (+) 0 tflags
  let indsE = scan (+) 0 eflags

  let s1 = if n > 0 then indsL[n-1] else 0
  let s2 = if n > 0 then indsE[n-1] else 0

  let inds  = map4 (\ c indL indE i ->
                        if c == 1 then indL - 1
                        else if c == 2 then s1 + indE - 1
                        else s1 + s2 + i - indsL[i] - indsE[i]
                   ) conds indsL indsE (iota n)
  in  (s1, s1+s2, inds)

def partition3 't [n] (p: t -> i8) (xs: [n]t)
   : {(i64,i64,[]t) | \(_, _, ys) ->
     let conds = map (\x -> p x) xs
     in FiltPart2 ys xs (\_i -> true) (\i -> conds[i] == 1) (\i -> conds[i] == 2)
   } =
  let conds = map (\x -> p x) xs
  let (a, b, inds) = partition3_indices conds
  let scratch = map (\x -> x) xs -- copy xs.
  let ys = scatter scratch inds xs
  in  (a, b, ys)


-- Based on work by Frederik Berthelsen, Kasper Erik
-- Schmidt-Christensen, Niels Hansen, and Mikkel Kragh Mathiesen.
--
-- Uses single precision floats.
--
-- It is a bit inefficient that we have to sort at the end to get the
-- right ordering, but the flattened quickhull does not otherwise
-- preserve it.
-- ==
-- tags { no_opencl }
-- compiled input @ data/2DinSphere_10K.in
-- output @ out.out

type real = f64
type dist = f64
type point = (f64, f64)

def zero_dist = 0f64
def dist_less (x : dist) (y : dist) = x < y

def point_eq (px: real, py: real) (qx: real, qy: real) =
  px == qx && py == qy

def point_less (px: real, py: real) (qx: real, qy: real) =
  px < qx || (px == qx && py < qy)

def sqr  (x : f64) = x * x
def ssqr (x : f64) = f64.abs x * x

def signed_dist_to_line (px: real, py: real) (qx: real, qy: real) (rx: real, ry: real) : dist =
  let ax = qx - px
  let ay = qy - py
  let bx = rx - px
  let by = ry - py
  in ssqr (ax * by - ay * bx) / (sqr ax + sqr ay)

-- def tabulate 't (n: i64) (f: i64 -> t): *[n]t =
--   map (\i -> f i) (iota n)

def max (i,id) (j,jd) =
  if dist_less jd id then (i,id) else (j,jd)

-- Precondition:  Range points.0 = [0, num_segs-1]
-- Postcondition: \ (xs, ys) -> length xs = 2*num_segs &&
--                              Range ys.0 = [0, 2*num_segs-1] &&
--                              Filt ys.1 points.1 (\_ -> false)
--                  -- ^ meaning some filtering with unknown pred
def expand_hull [num_segs] [num_points]
                (f_dist : (real, real) -> (real, real) -> (real, real) -> dist)
                (segs   : [num_segs](real,real,real,real))
                -- (segs_begx : [num_segs]real)
                -- (segs_begy : [num_segs]real)
                -- (segs_endx : [num_segs]real)
                -- (segs_endy : [num_segs]real)
                -- (points : [num_points](i64, real, real))
                (points_idx : {[num_points]i64 | \x -> Range x (0, num_segs)})
                (points_x : [num_points]real)
                (points_y : [num_points]real)
              : {( [](real,real,real,real) -- segs'
                , [](i64, real, real)     -- points'
                ) | \_ -> true} =
              -- : {([](point, point), [](i64, point)) | \_ -> true} =
  --
  let (segs_begx, segs_begy, segs_endx, segs_endy) = unzip4 segs
  -- let (points_idx, points_x, points_y) = unzip3 points

  -- bounds checks of array `segs` are verifiable by precondition
  let dists = map3
              (\ seg_ix px py ->
                 let ax = segs_begx[seg_ix]
                 let ay = segs_begy[seg_ix]
                 let bx = segs_endx[seg_ix]
                 let by = segs_endy[seg_ix]
                 in f_dist (ax, ay) (bx, by) (px, py)
              )
              points_idx points_x points_y
  
  let ne = (0, zero_dist)
  let bins = replicate num_segs ne
  let inds = points_idx
  let vals = zip (iota num_points) dists
  let extrema_ix = reduce_by_index bins max ne inds vals
  let (extrema_ix_inds, _extrema_ix_dsts) = unzip extrema_ix
  -- ^ 1. The length of `extrema_ix` is `num_segs` (from above)
  -- V 2. Bounds checks for `extrema_ix[i]` and `segs[i]` are verifiable,
  --      since 0 <= i < num_segs
  --   3. Bounds checks for `points[extrema_ix[i].0]` needs to prove that:
  --          0 <= extrema_ix[i].0 < num_points
  --      the neutral element (ne) for reduce_by_index is (0, zero_max)
  --      the operator returns one of the elements of the two args
  --      (in fist position),
  --       which are coming from `iota num_points` or from the neutral element 0
  --      Hence it should be able to prove the bounds above.
  --      If needed, modify `max` above to receive `num_points` as first (extra)
  --      arg,
  --        add the preconditions that `i` and `j` have range [0,num_points-1]
  --        and the postcondition for the first return as also
  --        Range [0,num_points-1]
  -- 
  let segs'' = map
                 (\i -> let pbx = segs_begx[i]
                        let pby = segs_begy[i]
                        let pext_x = points_x[ extrema_ix_inds[i] ]
                        let pext_y = points_y[ extrema_ix_inds[i] ]
                        let pex = segs_endx[i]
                        let pey = segs_endy[i]
                        in  ( [pbx, pext_x]
                            , [pby, pext_y]
                            , [pext_x, pex]
                            , [pext_y, pey]
                            )
                        -- [(segs[i].0, points[extrema_ix[i].0].1),
                        -- (points[extrema_ix[i].0].1, segs[i].1)]
                 )
                (iota num_segs)
  let (segs0'', segs1'', segs2'', segs3'') = unzip4 segs''
  let segs_begx' = flatten segs0''
  let segs_begy' = flatten segs1''
  let segs_endx' = flatten segs2''
  let segs_endy' = flatten segs3''
  -- let segs' = flatten segs''
  -- ^ Length of segs' is clearly `2*num_segs`

  -- let (sgm_inds, only_points) = unzip points
  let new_seg_inds =
    map4 (\ix seg_ix px py ->
        let extreme_ix = extrema_ix_inds[seg_ix]
        in if extreme_ix == ix then -1 else
        let ax = segs_begx[seg_ix]
        let ay = segs_begy[seg_ix]
        let bx = segs_endx[seg_ix]
        let by = segs_endy[seg_ix]
        let qx = points_x[extreme_ix]
        let qy = points_y[extreme_ix]
        let daq = f_dist (ax,ay) (qx,qy) (px,py)
        let dqb = f_dist (qx,qy) (bx,by) (px,py)
        in if dist_less zero_dist daq then (seg_ix * 2)
           else if dist_less zero_dist dqb then (seg_ix * 2 + 1)
           else (-1)
      ) (iota num_points) points_idx points_x points_y
  -- ^ From the map above, it should be provable that the range of
  --     `new_seg_inds` is [-1, 2*num_segs-1]
  -- V The filter below removes the less than zero elements, hence,
  --     the range of points'.0 should be `[0, 2*num_segs-1]`
  --   Finally, it should be trivial that `points'.1` is some filtering of `points.1`
  -- let points' = filter (\(i,_) -> i >= 0) (zip new_seg_inds only_points)
  let (n, is) = filter_indices (map (\i -> i >= 0) new_seg_inds)
  let zeros = replicate n 0i64
  let ids = scatter zeros is new_seg_inds
  let zeros = replicate n 0f64
  let points_x' = scatter zeros is points_x
  let zeros = replicate n 0f64
  let points_y' = scatter zeros is points_y
  in ( zip4 segs_begx' segs_begy' segs_endx' segs_endy'
     , zip3 ids points_x' points_y'
     )

--
-- Precondition: RANGE(sgm_inds) = [0,num_segs-1]
-- Postcondition: \ (_, segs', sgm_inds') ->
--                    Range sgm_inds' = [0, length segs' - 1] &&
--
def extract_empty_segments [num_segs] [num_points]
                           (hull : [](real,real))
                           (segs : [num_segs](real,real,real,real))
                           (sgm_inds : [num_points]i64)
    : ([](real,real), [](real,real,real,real), [num_points]i64) =
  let (segs_bx,segs_by,segs_ex,segs_ey) = unzip4 segs
  --
  -- V the content of `segs_inhabited` does not matter;
  --   it is a boolean array of length `num_segs` 
  let zeros = replicate num_segs 0
  let ones = replicate num_points 1
  let segs_inhabited' = reduce_by_index zeros (+) 0 sgm_inds ones
  let segs_inhabited = map (\i -> i > 0) segs_inhabited'

--  let segs_parted = scatter zeros inds segs  
  let (n, inds) = partition_indices segs_inhabited
  let zeros = replicate num_segs 0f64
  let segs_parted_bx = scatter zeros inds segs_bx
  let zeros = replicate num_segs 0f64
  let segs_parted_by = scatter zeros inds segs_by
  let zeros = replicate num_segs 0f64
  let segs_parted_ex = scatter zeros inds segs_ex
  let zeros = replicate num_segs 0f64
  let segs_parted_ey = scatter zeros inds segs_ey

--  let segs_true = segs_parted[:n]
  let segs_true_bx = segs_parted_bx[:n]
  let segs_true_by = segs_parted_by[:n]
  let segs_true_ex = segs_parted_ex[:n]
  let segs_true_ey = segs_parted_ey[:n]

--  let segs_false = segs_parted[n:]
  let segs_false_bx = segs_parted_bx[n:]
  let segs_false_by = segs_parted_by[n:]
  -- let segs_false_ex = segs_parted_ex[n:]
  -- let segs_false_ey = segs_parted_ey[n:]
  
  -- let (segs_true, segs_false) = partition (.1) (zip segs segs_inhabited)
  -- ^ we know that the length of `segs_true` is equal to the split point,
  --   which is is `SUM(segs_inhabited[0:num_segs-1])`
  
  let segs_indicator = map to_i64 segs_inhabited
  let sum_segs = scan (+) 0 segs_indicator
  let new_segs_ix = map2 (\i n -> n - i) segs_indicator sum_segs
  -- ^ IxFn for `new_segs_ix` is: 
  --   `forall i < num_segs . true => Sum(segs_inhabited[0:i-1])`
  
--  let hull' = (++) hull (map ((.0)) segs_false)
  let (hull_x, hull_y) = unzip hull
  let hull_x' = (++) hull_x segs_false_bx
  let hull_y' = (++) hull_y segs_false_by
  let hull'   = zip hull_x' hull_y'
-- 
--  let segs' = segs_true
  let segs' = zip4 segs_true_bx segs_true_by segs_true_ex segs_true_ey
  let sgm_inds' = 
    map (\seg_ix -> new_segs_ix[seg_ix] + segs_indicator[seg_ix] - 1) sgm_inds
  -- ^ Bounds check for `new_segs_ix[seg_ix]` should succeed, since
  --     precondition dictates `0 <= seg_ix < num_segs`
  --   Array `sgm_inds'` has index function:
  --       `forall i < num_segs . true => Sum(segs_inhabited[0 : sgm_inds[i]]) - 1`
  --   The postcondition reduces to proving that
  --       `Sum(segs_inhabited[0 : sgm_inds[i]]) - 1 < SUM(segs_inhabited[0 : num_segs-1])`
  --     which NOW (after the modification) works because it simplifies to
  --       `SUM(segs_inhabited[sgm_inds[i]+1 : num_segs-1]) + 1 > 0`
  --
  in (hull', segs', sgm_inds')

def semihull (startx: real, starty: real) (endx: real, endy: real) (points : [](real,real)) =
  if null points then [(startx,starty)]
  else
    -- establish the loop property that `RANGE(points.0) = [0, length segs-1]
    let (res, _, _) =
       loop (hull, segs, points) =
         ([], [(startx,starty, endx,endy)], map (\(x,y) -> (0, x, y)) points)
       while !null points do
         -- let (segs_begx, segs_begy, segs_endx, segs_endy) = unzip4 segs
         -- let (segs', points') = expand_hull segs_begx segs_begy segs_endx segs_endy points
         let (points_idx, points_x, points_y) = unzip3 points
         let (segs', points') = expand_hull signed_dist_to_line segs points_idx points_x points_y
         let (seg_inds', pointsx', pointsy') = unzip3 points'
         let (hull', segs'', sgm_inds'') = extract_empty_segments hull segs' seg_inds'
         in  (hull', segs'', zip3 sgm_inds'' pointsx' pointsy')
    in res

def pmin p q = if point_less p q then p else q
def pmax p q = if point_less p q then q else p

def compute (ps : []point)
    : {([](f64,f64), [](f64,f64)) | \_ -> true} =
  if length ps <= 3 then (ps, []) else
  let leftmost = reduce (\p q -> if point_less p q then p else q) ps[0] ps
  let rightmost = reduce (\p q -> if point_less p q then q else p) ps[0] ps
  -- let (_, upper_points, lower_points) =
  let (a, b, points_parted) =
    partition3
    (\p -> if point_eq p leftmost || point_eq p rightmost then 1 else if dist_less zero_dist (signed_dist_to_line leftmost rightmost p) then 2 else 0)
    ps
  let (_, upper_points, lower_points) = (points_parted[:a], points_parted[a:b], points_parted[b:])
  let upper_hull = semihull leftmost rightmost upper_points
  let lower_hull = semihull rightmost leftmost lower_points
  in (upper_hull, lower_hull)
