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
  : {(i64,i64,[n]i64) | \(a,b,is) ->
      FiltPartInv2 is (\_i -> true) (\i -> conds[i] == 1) (\i -> conds[i] == 2)
      -- ^ disjointness already proven by the above
      && Disjoint (\i -> (conds[i] == 1, conds[i] == 2, conds[i] != 1 && conds[i] != 2))
      && Range a (0,n) && Range b (0,n)
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

def partition3 't [n] (conds: [n]i8) (xs: [n]t)
   : {(i64,i64,[]t) | \(a, b, ys) ->
     FiltPart2 ys xs (\_i -> true) (\i -> conds[i] == 1) (\i -> conds[i] == 2)
     && Disjoint (\i -> (conds[i] == 1, conds[i] == 2, conds[i] != 1 && conds[i] != 2))
     && Range a (0,n) && Range b (0,n)
   } =
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

def signed_dist_to_line (px: real, py: real) (qx: real, qy: real) (rx: real, ry: real) : {dist | \_ -> true} =
  let ax = qx - px
  let ay = qy - py
  let bx = rx - px
  let by = ry - py
  in ssqr (ax * by - ay * bx) / (sqr ax + sqr ay)

-- def tabulate 't (n: i64) (f: i64 -> t): *[n]t =
--   map (\i -> f i) (iota n)

def max (i,id) (j,jd) =
  if dist_less jd id then (i,id) else (j,jd)

def mk2vec x y =
  map (\i -> if i == 0 then x else y) (iota 2)

def remove_negatives [num_points] num_segs (x: {[num_points]i64 | \x -> Range x (-1, 2*num_segs)}) points_x points_y
  : {([]i64, []real, []real) | \(y0,_,_) ->
    FiltPart y0 x (\i -> x[i] >= 0) (\_i -> true) && Range y0 (0, 2*num_segs)
  }  =
  let bs = map (\i -> i >= 0) x
  let (n, is) = filter_indices bs
  let zeros = replicate n 0i64
  let ids = scatter zeros is x
  let zeros = replicate n 0
  let points_x' = scatter zeros is points_x
  let zeros = replicate n 0
  let points_y' = scatter zeros is points_y
  in (ids, points_x', points_y')

-- Precondition:  Range points.0 = [0, num_segs-1]
-- Postcondition: \ (xs, ys) -> length xs = 2*num_segs &&
--                              Range ys.0 = [0, 2*num_segs-1] &&
--                              Filt ys.1 points.1 (\_ -> false)
--                  -- ^ meaning some filtering with unknown pred
def expand_hull [num_segs] [num_points]
                -- (segs   : [num_segs](real,real,real,real))
                (segs_begx : [num_segs]real)
                (segs_begy : [num_segs]real)
                (segs_endx : [num_segs]real)
                (segs_endy : [num_segs]real)
                -- (points : [num_points](i64, real, real))
                (points_idx : {[num_points]i64 | \x -> Range x (0, num_segs)})
                (points_x : [num_points]real)
                (points_y : [num_points]real)
              : {( []real,[]real,[]real,[]real -- segs'
                , []i64, []real, []real     -- points'
                ) | \(_,_,_,_, ys0,_,_) -> Range ys0 (0,2*num_segs)}
              =

  -- bounds checks of array `segs` are verifiable by precondition
  let dists = map3
              (\ seg_ix px py ->
                 let ax = segs_begx[seg_ix]
                 let ay = segs_begy[seg_ix]
                 let bx = segs_endx[seg_ix]
                 let by = segs_endy[seg_ix]
                 let axay = (ax, ay)
                 let bxby = (bx, by)
                 let pxpy = (px, py)
                 in signed_dist_to_line axay bxby pxpy
              )
              points_idx points_x points_y
  
  let ne = (0, zero_dist)
  let bins = replicate num_segs ne
  let inds = points_idx
  let x = iota num_points
  let vals = zip x dists
  let extrema_ix =
    reduce_by_index bins (\(i,id) (j,jd) -> if dist_less jd id then (i,id) else (j,jd)) ne inds vals
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
                        in  ( mk2vec pbx pext_x
                            , mk2vec pby pext_y
                            , mk2vec pext_x pex
                            , mk2vec pext_y pey
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
        let axay = (ax, ay)
        let bxby = (bx, by)
        let pxpy = (px, py)
        let qxqy = (qx, qy)
        let daq = signed_dist_to_line axay qxqy pxpy
        let dqb = signed_dist_to_line qxqy bxby pxpy
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
  let (ids, points_x', points_y') = remove_negatives num_segs new_seg_inds points_x points_y
  -- let (n, is) = filter_indices bs
  -- let zeros = replicate n 0i64
  -- let ids = scatter zeros is new_seg_inds
  -- let zeros = replicate n 0
  -- let points_x' = scatter zeros is points_x
  -- let zeros = replicate n 0
  -- let points_y' = scatter zeros is points_y
  in (segs_begx', segs_begy', segs_endx', segs_endy',
      ids, points_x', points_y')

def slice [n] 't (x: [n]t) (a: {i64 | \a' -> Range a' (0,inf)}) (b: {i64 | \b' -> Range b' (0,n+1)}) =
  map (\i -> x[i + a]) (iota (b - a))

def non_empty_segments
      [num_segs]
      [num_points]
      (_segs : [num_segs]real)
      (sgm_inds : [num_points]i64)
      : {[num_segs]bool | \_ -> true} =
  let zeros = replicate num_segs 0
  let ones = replicate num_points 1
  let segs_inhabited' = reduce_by_index zeros (+) 0 sgm_inds ones
  let segs_inhabited = map (\i -> i > 0) segs_inhabited'
  in segs_inhabited

--
-- Precondition: RANGE(sgm_inds) = [0,num_segs-1]
-- Postcondition: \ (_, segs', sgm_inds') ->
--                    Range sgm_inds' = [0, length segs' - 1] &&
--
def extract_empty_segments [num_segs] [num_points]
                           (hull_x : []real)
                           (hull_y : []real)
                           (segs_inhabited : [num_segs]bool)
                           (segs_bx : [num_segs]real)
                           (segs_by : [num_segs]real)
                           (segs_ex : [num_segs]real)
                           (segs_ey : [num_segs]real)
                           (sgm_inds : {[num_points]i64 | \x -> Range x (0, num_segs)})
    -- : {([](real,real), [](real,real,real,real), [num_points]i64) | \_ -> true} =
    : {( []real, []real              -- hull'
       , []real,[]real,[]real,[]real -- segs'
       , []i64                       -- seg_inds'
      ) | \(_,_, segs_true_bx,_,_,_, sgm_inds') ->
          let num_segs' = length segs_true_bx
          in Range sgm_inds' (0,num_segs')
      }
    =
  -- let segs_parted = scatter zeros inds segs  
  let (n, inds) = partition_indices segs_inhabited
  let zeros = replicate num_segs 0
  let segs_parted_bx = scatter zeros inds segs_bx
  let zeros = replicate num_segs 0
  let segs_parted_by = scatter zeros inds segs_by
  let zeros = replicate num_segs 0
  let segs_parted_ex = scatter zeros inds segs_ex
  let zeros = replicate num_segs 0
  let segs_parted_ey = scatter zeros inds segs_ey

  let zero = 0
  let segs_true_bx = slice segs_parted_bx zero n
  let segs_true_by = slice segs_parted_by zero n
  let segs_true_ex = slice segs_parted_ex zero n
  let segs_true_ey = slice segs_parted_ey zero n

  let segs_false_bx = slice segs_parted_bx n num_segs
  let segs_false_by = slice segs_parted_by n num_segs
  
  -- let (segs_true, segs_false) = partition (.1) (zip segs segs_inhabited)
  -- ^ we know that the length of `segs_true` is equal to the split point,
  --   which is is `SUM(segs_inhabited[0:num_segs-1])`
  
  let segs_indicator = map (\c -> if c then 1 else 0) segs_inhabited
  let sum_segs = scan (+) 0 segs_indicator
  let new_segs_ix = map2 (\i n -> n - i) segs_indicator sum_segs
  -- ^ IxFn for `new_segs_ix` is: 
  --   `forall i < num_segs . true => Sum(segs_inhabited[0:i-1])`
  
  let hull_x' = hull_x ++ segs_false_bx
  let hull_y' = hull_y ++ segs_false_by
  -- let hull'   = zip hull_x' (sized_like hull_x' hull_y')

  -- let segs' = zip4 segs_true_bx segs_true_by segs_true_ex segs_true_ey
  let sgm_inds' = 
      -- map (\seg_ix -> new_segs_ix[seg_ix] + segs_indicator[seg_ix] - 1) sgm_inds
      map (\seg_ix -> if segs_inhabited[seg_ix]
                      then new_segs_ix[seg_ix]
                      else 0
          ) sgm_inds
  -- ^ Bounds check for `new_segs_ix[seg_ix]` should succeed, since
  --     precondition dictates `0 <= seg_ix < num_segs`
  --   Array `sgm_inds'` has index function:
  --       `forall i < num_segs . true => Sum(segs_inhabited[0 : sgm_inds[i]]) - 1`
  --   The postcondition reduces to proving that
  --       `Sum(segs_inhabited[0 : sgm_inds[i]-1]) < SUM(segs_inhabited[0 : num_segs-1])`
  --   After subtraction it becomes:
  --       `Sum(segs_inhabited[sgm_inds[i] : num_segs-1]) > 0`
  --   After expanding with known indices it becomes:
  --       `1 + Sum(segs_inhabited[sgm_inds[i] : num_segs-1]) > 0`
  --     which NOW (after the modification) works!
  --
  in (hull_x', hull_y', segs_true_bx, segs_true_by, segs_true_ex, segs_true_ey, sgm_inds')

def semihull_loop [num_segs] [num_points]
                  (hull_x : []real)
                  (hull_y : []real)
                  -- (segs   : [num_segs](real,real,real,real))
                  (segs_begx : [num_segs]real)
                  (segs_begy : [num_segs]real)
                  (segs_endx : [num_segs]real)
                  (segs_endy : [num_segs]real)
                  -- (points : [num_points](i64, real, real))
                  (points_idx : {[num_points]i64 | \x -> Range x (0,num_segs)})
                  (points_x : [num_points]real)
                  (points_y : [num_points]real)
    : {( []real, []real              -- hull'
       , []real,[]real,[]real,[]real -- segs'
       , []i64,[]real,[]real         -- points
      ) | \(_,_, out_segs_begx,_,_,_, out_points_idx,_,_) -> Range out_points_idx (0,length out_segs_begx)}
    =
   let (segs_begx', segs_begy', segs_endx', segs_endy',
        points_idx', points_x', points_y') = expand_hull segs_begx segs_begy segs_endx segs_endy points_idx points_x points_y
   let segs_inhabited = non_empty_segments segs_begx' points_idx'
   let (hull_x', hull_y', segs_true_bx, segs_true_by, segs_true_ex, segs_true_ey, points_idx'') = extract_empty_segments hull_x hull_y segs_inhabited segs_begx' segs_begy' segs_endx' segs_endy' points_idx'
   in  (hull_x', hull_y', segs_true_bx, segs_true_by, segs_true_ex, segs_true_ey, points_idx'', points_x', points_y')

def semihull [n] (startx: real, starty: real) (endx: real, endy: real) (points0 : [n]real) (points1 : [n]real) : {[](real, real) | \_ -> true}  =
  -- We don't support branches with different sizes in the index
  -- function implementation yet, so I comment out the case
  -- that handles empty inputs below.
  --
  -- if n == 0 then map (\_ -> (startx,starty)) (iota 1)
  -- else
    let hull = map (\_ -> (0,0)) (iota 0)
    let segs = map (\_ -> (startx,starty, endx,endy)) (iota 1)
    let points_idx = map (\_ -> 0) points0
    let points_x = points0
    let points_y = points1
    let (segs_begx, segs_begy, segs_endx, segs_endy) = unzip4 segs
    let (hullx, hully) = unzip hull
    let (hullx', hully', _,_,_,_, _,_,_) =
       -- loop (hull, segs, points) =
       --   ([], [(startx,starty, endx,endy)], map (\(x,y) -> (0, x, y)) points)
       -- while !null points do
         semihull_loop hullx hully segs_begx segs_begy segs_endx segs_endy points_idx points_x points_y
    let hull' = zip hullx' (sized_like hullx' hully')
    in hull'

def pmin p q = if point_less p q then p else q
def pmax p q = if point_less p q then q else p

def get_leftmost_rightmost (n: {i64 | \n -> Range n (1,inf)}) (ps0: [n]f64) (ps1: [n]f64): {((f64, f64), (f64, f64)) | \_ -> true} =
  let ps = zip ps0 ps1
  let ne = ps[0]
  let leftmosts = scan (\(p1,p2) (q1,q2) -> if point_less (p1,p2) (q1,q2) then (p1,p2) else (q1,q2)) ne ps
  let rightmosts = scan (\(p1,p2) (q1,q2) -> if point_less (p1,p2) (q1,q2) then (p1,p2) else (q1,q2)) ne ps
  let (leftmosts1, leftmosts2) = unzip leftmosts
  let (rightmosts1, rightmosts2) = unzip rightmosts
  in ((leftmosts1[n-1], leftmosts2[n-1]), (rightmosts1[n-1], rightmosts2[n-1]))

def compute (n: {i64 | \n -> Range n (3,inf)}) (ps0 : [n]f64)
    (ps1 : [n]f64)
    : {([](f64,f64), [](f64,f64)) | \_ -> true} =
  let ((leftmost1, leftmost2), (rightmost1, rightmost2)) = get_leftmost_rightmost n ps0 ps1
  let left = (leftmost1,leftmost2)
  let right = (rightmost1,rightmost2)
  let conds = map2 (\p1 p2 ->
      let p = (p1,p2)
      in if point_eq p left || point_eq p right
      then 1
      else let d = signed_dist_to_line left right p
           in if dist_less zero_dist d
           then 2
           else 0
    ) ps0 ps1
  let (a, b, points_parted0) = partition3 conds ps0
  let (_, _, points_parted1) = partition3 conds ps1
  -- let _discard = slice points_parted 0 a
  let upper_points0 = slice points_parted0 a b
  let upper_points1 = slice points_parted1 a b
  let lower_points0 = slice points_parted0 b n
  let lower_points1 = slice points_parted1 b n
  let upper_hull = semihull left right upper_points0 upper_points1
  let lower_hull = semihull right left lower_points0 lower_points1
  in (upper_hull, lower_hull)
