--
-- Prelude
--
def mk_flag_array 't 'a [m]
    (shape: {[m]i64 | \x -> Range x (0,inf)})
    (zero: t)
    (xs: [m]t)
    : {[]t | \_ -> true} =
  let shp_rot = map (\i -> if i==0 then 0i64 else shape[i-1]) (iota m)
  let shp_scn = scan (+) 0i64 shp_rot
  let shp_ind =
        map2 (\ shp ind ->
                if shp <= 0i64 then -1i64 else ind
             ) shape shp_scn
  let aoa_len = if m > 0 then shp_scn[m-1] + shape[m-1] else 0
  let zeros = replicate aoa_len zero
  let res = scatter zeros shp_ind xs
  in  res

def sgm_sum [n] 't
    (flags: [n]bool)
    (xs: [n]i64)
    : {[n]i64 | \_ -> true} =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys

def segment_ids [m]
    (shape: {[m]i64 | \x -> Range x (0,inf)})
    : {([]i64, []bool) | \(ids, _) -> Range ids (0,m)} =
  let flags1 = map (\i -> i + 1) (iota m)
  let zero = 0
  let flags = mk_flag_array shape zero flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  in (sgm_sum flags_bool flags_sgmind, flags_bool)

def repl_segm_iota [m]
    (shape: {[m]i64 | \x -> Range x (0,inf)})
    : {([]i64, []i64) | \(ids, _) -> Range ids (0,m)} =
  let (ids, flags) = segment_ids shape
  let ones = map (\_ -> 1) flags
  let tmp = sgm_sum flags ones
  let iotas = map (\x -> x - 1) tmp
  in (ids, iotas)

--
-- Program
--

-- Helpers
let to_i64 (b: bool) : i64 = i64.bool b

-- Top-level filter function for Structure of Arrays (SoA)
-- Returns unique arrays (*[]i64)
def filter_soa [n] (flags: [n]bool) (arr1: [n]i64) (arr2: [n]i64) : (*[]i64, *[]i64) =
  let num_trues = scan (+) 0 (map (\c -> to_i64 c) flags)
  let new_size = if n > 0 then num_trues[n-1] else 0
  let ranks = map2 (\c i -> if c then i-1 else -1) flags num_trues
  let out1 = replicate new_size 0i64
  let out2 = replicate new_size 0i64
  let out1 = scatter out1 ranks arr1
  let out2 = scatter out2 ranks arr2
  in (out1, out2)

def remove_duplicates [Q] (V: i64) (q_verts: [Q]i64) (q_parents: [Q]i64): (*[]i64, *[]i64) = 
  let indexes = iota Q 
  let H = hist i64.min Q V q_verts indexes
  let flags = map2 (\i j -> H[i] == j) q_verts indexes
  in filter_soa flags q_verts q_parents

def update_parents [V] [Q] (parents: *[V]i64) (q_verts: [Q]i64) (q_parents: [Q]i64): *[V]i64 =
  scatter parents q_verts q_parents

def make_shape [V] [Q]
    (offsets: {[V+1]i64 | \x -> Range x (0,inf) && Monotonic (<=) x})
    (queue: {[Q]i64 | \x -> Range x (0,V)})
    : {[Q]i64 | \y ->
      For y (\i -> Range y (0, offsets[queue[i]+1] - offsets[queue[i]] + 1))
    } =
  map (\q -> offsets[q+1] - offsets[q]) queue

def expand [V] [E] [Q]
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (edges: {[E]i64 | \x -> Range x (0, V)})
    (queue: {[Q]i64 | \x -> Range x (0,V)})
    : {(*[]i64, *[]i64) | \_ -> true} =
  -- 1. Calculate vertex degrees
  let q_shape = map (\q -> offsets[q+1] - offsets[q]) queue
  let (ids, iotas) = repl_segm_iota q_shape
  let q_ids = map (\i -> queue[i]) ids
  let q_offsets = map (\i -> offsets[queue[i]]) ids

  -- 2. Generate edges
  let indices = map2 (\offset j -> offset + j) q_offsets iotas
  let q_edges = map (\i -> edges[i]) indices
  in (q_ids, q_edges)

def bfs_step [V] [E] [Q]
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (edges: {[E]i64 | \x -> Range x (0, V)})
    (parents: *[V]i64)
    (queue: {[Q]i64 | \x -> Range x (0,V)})
    : {(*[V]i64, *[]i64) | \_ -> true} =
  let (raw_parents, raw_verts) = expand offsets edges queue
  let valid_flags = map (\v -> parents[v] == -1) raw_verts
  let (filtered_v, filtered_p) = filter_soa valid_flags raw_verts raw_parents
  let (noDupes_v, noDupes_p) = remove_duplicates V filtered_v filtered_p
  let new_parents = update_parents parents noDupes_v noDupes_p
  in (new_parents, noDupes_v)

-- def BFS [V] [E] [Q]
--   (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
--   (edges: {[E]i64 | \x -> Range x (0, V)})
--   (parents: *[V]i64)
--   (queue: [Q]i64)
--   : {[V]i64 | \_ -> true} =
--     let (parents, _) = loop (parents, queue) while length queue > 0 do
--         -- This copy is not needed on the main-branch compiler, but has nothing
--         -- to do with our system; our annotations just haven't been integrated
--         -- fully with the handling of uniqueness types in the type checker,
--         -- meaning we are not able to mark bfs_step's returns unique.
--         let p = copy parents
--         in bfs_step offsets edges p queue
--     in parents
