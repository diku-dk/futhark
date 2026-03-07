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
      && Range a (0,n+1) && Range b (0,n+1)
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
     && Range a (0,n+1) && Range b (0,n+1)
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

def compute_new_seg_inds [num_segs] [num_points]
    (segs_begx : [num_segs]real)
    (segs_begy : [num_segs]real)
    (segs_endx : [num_segs]real)
    (segs_endy : [num_segs]real)
    (points_idx : {[num_points]i64 | \x -> Range x (0, num_segs)})
    (points_x : [num_points]real)
    (points_y : [num_points]real)
    (extrema_ix_inds : {[num_segs]i64 | \x -> Range x (0, num_points)})
    : ({[num_points]i64 | \y -> Range y (-1, 2 * num_segs)}) =
  map4 (\ix seg_ix px py ->
      let extreme_ix = extrema_ix_inds[seg_ix]
      in if extreme_ix == ix then -1i64 else
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

def expand_hull [num_segs] [num_points]
    -- (segs   : [num_segs](real,real,real,real))
    (segs_begx : [num_segs]real)
    (segs_begy : [num_segs]real)
    (segs_endx : [num_segs]real)
    (segs_endy : [num_segs]real)
    -- (points : [num_points](i64, real, real))
    (points_idx : {[num_points]i64 | \x -> Range x (0, length segs_begx)})
    (points_x : [num_points]real)
    (points_y : [num_points]real)
    : {( []real,[]real,[]real,[]real -- segs'
       , []i64, []real, []real       -- points'
      ) | \(segs_bx',_,_,_, points_idx',_,_) ->
        Range points_idx' (0, length segs_bx')
    } =
  let dists = map3 (\seg_ix px py ->
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
  let segs'' = map (\i ->
      let pbx = segs_begx[i]
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
    )
    (iota num_segs)
  let (segs0'', segs1'', segs2'', segs3'') = unzip4 segs''
  let segs_begx' = flatten segs0''
  let segs_begy' = flatten segs1''
  let segs_endx' = flatten segs2''
  let segs_endy' = flatten segs3''

  let new_seg_inds =
    compute_new_seg_inds segs_begx segs_begy segs_endx segs_endy points_idx points_x points_y extrema_ix_inds
  let (ids, points_x', points_y') = remove_negatives num_segs new_seg_inds points_x points_y
  in (segs_begx', segs_begy', segs_endx', segs_endy',
      ids, points_x', points_y')

def slice [n] 't (x: [n]t) (a: {i64 | \a' -> Range a' (0,inf)}) (b: {i64 | \b' -> Range b' (0,n+1)}) =
  map (\i -> x[i + a]) (iota (b - a))

def extract_empty_segments [num_segs] [num_points]
    (hull_x : []real)
    (hull_y : []real)
    (segs_bx : [num_segs]real)
    (segs_by : [num_segs]real)
    (segs_ex : [num_segs]real)
    (segs_ey : [num_segs]real)
    (sgm_inds : {[num_points]i64 | \x -> Range x (0, length segs_bx)})
    : {( []real, []real              -- hull'
       , []real,[]real,[]real,[]real -- segs'
       , []i64                       -- seg_inds'
       ) | \(_,_, segs_bx',_,_,_, sgm_inds') ->
         Range sgm_inds' (0, length segs_bx')
      } =
  let zeros = replicate num_segs 0
  let ones = replicate num_points 1
  let seg_sizes = reduce_by_index zeros (+) 0 sgm_inds ones
  let segs_inhabited = map (\i -> i > 0) seg_sizes

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
  
  let hull_x' = hull_x ++ segs_false_bx
  let hull_y' = hull_y ++ segs_false_by

  let segs_indicator = map (\c -> if c then 1 else 0) segs_inhabited
  let sum_segs = scan (+) 0 segs_indicator

  let sgm_inds' = map (\seg_ix ->
      if segs_inhabited[seg_ix]
      then sum_segs[seg_ix] - segs_indicator[seg_ix]
      else 0
    ) sgm_inds
  in (hull_x', hull_y', segs_true_bx, segs_true_by, segs_true_ex, segs_true_ey, sgm_inds')

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
  let (segs_bx, segs_by, segs_ex, segs_ey) = unzip4 segs
  let (hull_x, hull_y) = unzip hull
  let (hull_x', hull_y', _,_,_,_, _,_,_) =
     -- loop (hull, segs, points) =
     --   ([], [(startx,starty, endx,endy)], map (\(x,y) -> (0, x, y)) points)
     -- while !null points do
    -- Loop invariant is:
    --   points_idx : [num_points]i64 | Range points_idx (0, length segs_begx)
    -- which is annotated as both pre- and postconditions
    -- on both expand_hull and extract_empty_segments.
    let ( segs_bx'
        , segs_by'
        , segs_ex'
        , segs_ey'
        , points_idx'
        , points_x'
        , points_y') =
      expand_hull
        segs_bx
        segs_by
        segs_ex
        segs_ey
        points_idx
        points_x
        points_y
    let ( hull_x'
        , hull_y'
        , segs_true_bx
        , segs_true_by
        , segs_true_ex
        , segs_true_ey
        , points_idx'') =
      extract_empty_segments
        hull_x
        hull_y
        segs_bx'
        segs_by'
        segs_ex'
        segs_ey'
        points_idx'
    in  (hull_x', hull_y', segs_true_bx, segs_true_by, segs_true_ex, segs_true_ey, points_idx'', points_x', points_y')
  let hull' = zip hull_x' (sized_like hull_x' hull_y')
  in hull'

def pmin p0 p1 q0 q1: {(f64,f64) | \_ -> true} =
  -- Applying the identity function makes the index function uninterpreted.
  (\x -> x) (if point_less (p0,p1) (q0,q1) then (p0,p1) else (q0,q1))
def pmax p0 p1 q0 q1: {(f64,f64) | \_ -> true} =
  (\x -> x) (if point_less (p0,p1) (q0,q1) then (q0,q1) else (p0,p1))

def get_leftmost (n: {i64 | \n -> Range n (1,inf)}) (ps0: [n]f64) (ps1: [n]f64): {(f64, f64) | \_ -> true} =
  let ps = zip ps0 ps1
  let leftmosts = scan (\(p0,p1) (q0,q1) -> pmin p0 p1 q0 q1) (ps0[0], ps1[0]) ps
  in leftmosts[n-1]

def get_rightmost (n: {i64 | \n -> Range n (1,inf)}) (ps0: [n]f64) (ps1: [n]f64): {(f64, f64) | \_ -> true} =
  let ps = zip ps0 ps1
  let rightmosts = scan (\(p0,p1) (q0,q1) -> pmax p0 p1 q0 q1) (ps0[0], ps1[0]) ps
  in rightmosts[n-1]

def compute (n: {i64 | \n -> Range n (3,inf)}) (ps0 : [n]f64)
    (ps1 : [n]f64)
    : {([](f64,f64), [](f64,f64)) | \_ -> true} =
  let (leftmost1, leftmost2) = get_leftmost n ps0 ps1
  let (rightmost1, rightmost2) = get_rightmost n ps0 ps1
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
  let upper_points0 = slice points_parted0 a b
  let upper_points1 = slice points_parted1 a b
  let lower_points0 = slice points_parted0 b n
  let lower_points1 = slice points_parted1 b n
  let upper_hull = semihull left right upper_points0 upper_points1
  let lower_hull = semihull right left lower_points0 lower_points1
  in (upper_hull, lower_hull)
