def (++) 't [n][m] (xs: [n]t) (ys: [m]t): *[n+m]t =
  tabulate (n+m) (\i -> if i < n then xs[i] else ys[i-n])

def sum [n] (xs: [n]i64) =
  if n > 0 then (scan (+) 0 xs)[n-1] else 0

def to_i64 c : i64 = if c then 1 else 0

def filter_indices [n]
  (cs: [n]bool)
  -- : (i64, [n]i64) =
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
  -- : (i64, [n]i64) =
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

def partition3_indices [n] 't (conds: [n]i8) : {(i64,i64,[n]i64) | \(_,_,is) ->
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

type dist = f64
type point = [2]f64

def zero_dist = 0f64
def dist_less (x : dist) (y : dist) = x < y

def point_eq (p : point) (q : point) =
  p[0] == q[0] && p[1] == q[1]

def point_less (p : point) (q : point) =
  p[0] < q[0] || (p[0] == q[0] && p[1] < q[1])

def sqr (x : f64) = x * x
def ssqr (x : f64) = f64.abs x * x

def signed_dist_to_line (p : point) (q : point) (r : point) : dist =
  let ax = q[0] - p[0]
  let ay = q[1] - p[1]
  let bx = r[0] - p[0]
  let by = r[1] - p[1]
  in ssqr (ax * by - ay * bx) / (sqr ax + sqr ay)

def tabulate 't (n: i64) (f: i64 -> t): *[n]t =
  map (\i -> f i) (iota n)

-- Precondition:  Range points.0 = [0, num_segs-1]
-- Postcondition: \ (xs, ys) -> length xs = 2*num_segs &&
--                              Range ys.0 = [0, 2*num_segs-1] &&
--                              Filt ys.1 points.1 (\_ -> false)
--                  -- ^ meaning some filtering with unknown pred
def expand_hull [num_segs] [num_points]
                (segs0 : [num_segs]point)
                (segs1 : [num_segs]point)
                (points0 : {[num_points]i64 | \x -> Range x (0, num_segs)})
                (points1 : [num_points]point)
              : {([](point, point), [](i64, point)) | \_ -> true} =
  --
  -- bounds checks of array `segs` are verifiable by precondition
  let segs = zip segs0 segs1
  let points = zip points0 points1
  let dists = map
              (\(seg_ix, p) ->
                 let a = segs0[seg_ix]
                 let b = segs1[seg_ix]
                 in signed_dist_to_line a b p)
              points
  
  let max (i,id) (j,jd) =
    if dist_less jd id then (i,id) else (j,jd)

  let ne = (0, zero_dist)
  let bins = replicate num_segs ne
  let inds = map (\(i,_) -> i) points
  let vals = zip (iota num_points) dists
  let extrema_ix = reduce_by_index bins max ne inds vals
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
  let segs'' = tabulate num_segs
                       (\i -> [(segs[i].0, points[extrema_ix[i].0].1),
                               (points[extrema_ix[i].0].1, segs[i].1)])
  let segs' = flatten segs''
  -- ^ Length of segs' is clearly `2*num_segs`

  let (sgm_inds, only_points) = unzip points
  let new_seg_inds =
    map3 (\ix seg_ix p ->
        let (extreme_ix, _) = extrema_ix[seg_ix]
        in if extreme_ix == ix then -1 else
        let (a, b) = segs[seg_ix]
        let q = points[extreme_ix].1
        let daq = signed_dist_to_line a q p
        let dqb = signed_dist_to_line q b p
        in if dist_less zero_dist daq then (seg_ix * 2)
           else if dist_less zero_dist dqb then (seg_ix * 2 + 1)
           else (-1)
      ) (iota num_points) sgm_inds only_points
  -- ^ From the map above, it should be provable that the range of
  --     `new_seg_inds` is [-1, 2*num_segs-1]
  -- V The filter below removes the less than zero elements, hence,
  --     the range of points'.0 should be `[0, 2*num_segs-1]`
  --   Finally, it should be trivial that `points'.1` is some filtering of `points.1`
  -- let points' = filter (\(i,_) -> i >= 0) (zip new_seg_inds only_points)
  let (n, is) = filter_indices (map (\i -> i >= 0) new_seg_inds)
  let zeros = replicate n 0i64
  let ids = scatter zeros is new_seg_inds
  let zeros = replicate n (map (\_ -> 0f64) (iota 2))
  let only_points' = scatter zeros is only_points
  let points' = zip ids only_points'
  in (segs', points')

--
-- Precondition: RANGE(sgm_inds) = [0,num_segs-1]
-- Postcondition: \ (_, segs', sgm_inds') ->
--                    Range sgm_inds' = [0, length segs' - 1] &&
--
def extract_empty_segments [num_segs] [num_points]
                           (hull : [](point))
                           (segs : [num_segs](point, point))
                           (sgm_inds : [num_points]i64)
    : ([](point), [](point, point), [num_points]i64) =
  let point_ixs = sgm_inds

  -- V the content of `segs_inhabited` does not matter;
  --   it is a boolean array of length `num_segs` 
  let zeros = replicate num_segs 0
  let ones = replicate num_points 1
  let segs_inhabited' = reduce_by_index zeros (+) 0 point_ixs ones
  let segs_inhabited = map (\i -> i > 0) segs_inhabited'
  
  let (n, inds) = partition_indices segs_inhabited
  let zeros = replicate num_segs (map (\_ -> 0f64) (iota 2), map (\_ -> 0f64) (iota 2))
  let segs_parted = scatter zeros inds segs
  let segs_true = segs_parted[:n]
  let segs_false = segs_parted[n:]
  -- let (segs_true, segs_false) = partition (.1) (zip segs segs_inhabited)
  -- ^ we know that the length of `segs_true` is equal to the split point,
  --   which is is `SUM(segs_inhabited[0:num_segs-1])`
  
  let segs_indicator = map to_i64 segs_inhabited
  let sum_segs = scan (+) 0 segs_indicator
  let new_segs_ix = map2 (\i n -> n - i) segs_indicator sum_segs
  -- ^ IxFn for `new_segs_ix` is: 
  --   `forall i < num_segs . true => Sum(segs_inhabited[0:i-1])`
  
  let hull' = (++) hull (map ((.0)) segs_false)
  let segs' = segs_true
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

def semihull (start : point) (end : point) (points : []point) =
  if null points then [start]
  else
    -- establish the loop property that `RANGE(points.0) = [0, length segs-1]
    (loop (hull, segs, points) =
       ([], [(start, end)], map (\p -> (0, p)) points)
     while !null points do
     let (segs0, segs1) = unzip segs       -- Workaround: implementation doesn't support tuple projection.
     let (points0, points1) = unzip points -- Workaround: implementation doesn't support tuple projection.
     let (segs', points') = expand_hull segs0 segs1 points0 points1
     let (seg_inds', only_points') = unzip points'
     let (hull', segs'', sgm_inds'') = extract_empty_segments hull segs' seg_inds'
     in  (hull', segs'', zip sgm_inds'' only_points')
    ) |> (.0)

def pmin p q = if point_less p q then p else q
def pmax p q = if point_less p q then q else p

def compute (ps : []point) =
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


-- import "lib/github.com/diku-dk/sorts/radix_sort"
def sort_by [n] 't (_f: t -> f64) (_xs: [n]t): [n]t = ??? -- radix_sort_float_by_key f f64.num_bits f64.get_bit

def clockwise (convex_upper: []point) (convex_lower: []point) =
  let sorted_upper = sort_by (\p -> p[0]) (sort_by (\p -> p[1]) convex_upper)
  let sorted_lower = sort_by (\p -> p[0]) (sort_by (\p -> p[1]) convex_lower)
  let upper_is = sorted_upper
  let lower_is = (reverse sorted_lower)
  in upper_is++lower_is

entry main [k] (ps : [k][2]f64) : [][2]f64 =
  let (convex_upper, convex_lower) = compute ps
  in clockwise convex_upper convex_lower
