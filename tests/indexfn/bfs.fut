
-- Helpers
let to_i64 (b: bool) : i64 = i64.bool b

let sum [n] (xs: [n]i64) = if n > 0 then (scan (+) 0 xs)[n-1] else 0


-- Creates an array where 'xs' are scattered to the start positions defined by 'shape'.
-- Annotated with *[]t to indicate a unique return array.
def mk_flag_array 't [m]
        (shape: {[m]i64 | \x -> Range x (0,inf)})
        (zero: t)
        (xs: [m]t)
        : {*[]t | \flags -> length flags == sum shape} =
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

-- Segmented Sum (Prefix Sum)
def sgm_sum [n]
      (flags: [n]bool)
      (xs: [n]i64): {[n]i64 | \_ -> true} =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x + y))
         (false, 0i64)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys

-- Segmented Copy
def sgm_copy [n] 't
      (flags: [n]bool)
      (zero: t)
      (xs: [n]t): {[n]t | \_ -> true} =
  let zipped = zip flags xs
  let flags_ys =
    scan (\(x_flag,x) (y_flag,y) ->
           (x_flag || y_flag,
            if y_flag then y else x))
         (false, zero)
         zipped
  let (_flags, ys) = unzip flags_ys
  in ys

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

def remove_duplicates [nQueue] (nVerts: i64) (q_verts: [nQueue]i64) (q_parents: [nQueue]i64): (*[]i64, *[]i64) =
    let indexes = iota nQueue

    let H = hist i64.min nQueue nVerts q_verts indexes

    let flags = map2 (\i j -> H[i] == j) q_verts indexes

    in filter_soa flags q_verts q_parents

def update_parents [nVerts] [nQueue] (parents: *[nVerts]i64) (q_verts: [nQueue]i64) (q_parents: [nQueue]i64): *[nVerts]i64 =
    scatter parents q_verts q_parents

-- def extend [nVerts] (nEdges: i64) (verts: {[nVerts]i64 | \v -> Range v (0,nVerts)k}) =
--   let singleton = map (\_ -> nEdges) (iota 1)
--   in verts ++ singleton

def get_queued_edges_shape [nVerts] [nEdges] [nQueue]
    (verts: {[nVerts]i64 | \v -> Range v (0,nEdges) && Monotonic (<=) v})
    (_edges: {[nEdges]i64 | \e -> Range e (0, nVerts)})
    (queue_v: {[nQueue]i64 | \v -> Range v (0, nVerts)})
    -- : {*[nQueue]i64 | \r -> Range r (0,nEdges)} =
    : {*[nQueue]i64 | \r -> Range r (0,inf)} =
    let shape = map (\i ->
        if i + 1 < nVerts then verts[i+1] - verts[i] else nEdges - verts[i]
      ) (iota nVerts)
    let counts = map (\q -> shape[q]) queue_v
    in counts


-- Specialized Expand Function
--  expand
--      (\v ->
--          let extended = verts ++ [nEdges]
--          in extended[v + 1] - extended[v])
--      (\v i ->
--          let currentVert = edges[verts[v] + i]
--          in if (parents[currentVert] == -1)
--          then (currentVert, v)
--          else (-1, -1))
--      queue_v
-- Merges the generic expand logic with the specific edge counting and lookup logic because we don't support lambdas as first-class citizens in the prototype.
def expand_edge_count [nVerts] [nEdges] [nQueue]
    -- (verts: {[nVerts]i64 | \v -> Range v (0, nVerts)})
    (verts: {[nVerts]i64 | \v -> Range v (0,nEdges) && Monotonic (<=) v})
    (edges: {[nEdges]i64 | \e -> Range e (0, nVerts)})
    (parents: []i64)
    (queue_v: {[nQueue]i64 | \v -> Range v (0, nVerts)})
    : {*[](i64, i64) | \_ -> true} =

    -- 1. Calculate sizes (vertex degrees)
    -- let shape = map (\i ->
    --     if i + 1 < nVerts then verts[i+1] - verts[i] else nEdges - verts[i]
    --   ) (iota nVerts)
    -- let counts = map (\q -> shape[q]) queue_v
    let counts = get_queued_edges_shape verts edges queue_v

    -- 2. Create Seeds and Flags together
    -- Optimization: We call mk_flag_array once. We use -1 as the dummy value.
    -- Since queue_v contains non-negative vertex IDs, -1 indicates a "gap".
    let dummy = -1i64
    let seeds = mk_flag_array counts dummy queue_v

    -- Derive flags from seeds: any valid vertex ID (>= 0) marks the start of a segment.
    let flags = map (\x -> x != -1) seeds

    -- 3. Segmented Replicate (queue vertices)
    -- We use -1 as the neutral element, but it is immediately overwritten
    -- by the seed values at flag positions.
    let seeds' = map (\x -> if x != -1 then x else 0) seeds
    let flat_v = sgm_sum flags seeds'

    -- 4. Segmented Iota (edge indices)
    let ones = map (\_ -> 1) seeds
    let flat_idxs = map (\x -> x - 1) (sgm_sum flags ones)

    -- 5. Generate edges and check parents
    in map2 (\v i ->
        let currentVert = edges[verts[v] + i]
        in if (parents[currentVert] == -1)
        then (currentVert, v)
        else (-1, -1)
    ) flat_v flat_idxs

def BFS_loop_body [nVerts] [nEdges] [nQueue] (verts: [nVerts]i64) (edges: [nEdges]i64) (parents: *[nVerts]i64) (queue_v: [nQueue]i64): (*[nVerts]i64, *[]i64) =
    let next_step_aot = expand_edge_count verts edges parents queue_v
    let (raw_verts, raw_parents) = unzip next_step_aot
    let valid_flags = map (\p -> p != -1) raw_parents
    let (filtered_v, filtered_p) = filter_soa valid_flags raw_verts raw_parents
    let (noDupes_v, noDupes_p) = remove_duplicates nVerts filtered_v filtered_p
    in (update_parents parents noDupes_v noDupes_p, noDupes_v)

def BFS [nVerts] [nEdges] [nQueue] (verts: [nVerts]i64) (edges: [nEdges]i64) (parents: *[nVerts]i64) (queue_v: [nQueue]i64): [nVerts]i64 =

    let (parents, _) = loop (parents, queue_v) while length queue_v > 0 do
        BFS_loop_body verts edges parents queue_v
    in parents

def fromi32 x = i64.i32 x

-- Input uses the Adjacency graph description:
--   https://cmuparlay.github.io/pbbsbench/fileFormats/graph.html
--
-- Moreover, edges_enc is monotonic within each segment defined by
-- vertexes_enc. (In the prototype compiler, we cannot encode this yet,
-- but the paper would use the `For` property.)
def main [nVerts] [nEdges] (vertexes_enc: {[nVerts]i32 | \v -> Range v (0,nEdges) && Monotonic (<=) v}) (edges_enc: {[nEdges]i32 | \e -> Range e (0, nVerts)}): {[]i32 | \_ -> true} =
    let vertexes_64 = map (\x -> i64.i32 x) vertexes_enc
    let edges_64 = map (\x -> i64.i32 x) edges_enc

    let start = 0i64
    let parents = replicate nVerts (-1i64)

    let q_verts = map (\_ -> start) (iota 1)
    let q_parents = map (\_ -> start) (iota 1)

    let parents = update_parents parents q_verts q_parents

    let parents_64 = BFS vertexes_64 edges_64 parents q_verts

    in map i32.i64 parents_64

-- ==
-- input @ randLocalGraph_J_10_20000000.in
-- output @ randLocalGraph_J_10_20000000.out
