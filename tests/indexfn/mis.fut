-- import "lib/github.com/diku-dk/cpprandom/random"
-- import "lib/github.com/diku-dk/cpprandom/shuffle"

-- module shuffle = mk_shuffle minstd_rand

let mk_flag_array 't 'a [m]
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

-- Expands a shape array to flat arrays of segment ids and flags.
let segment_ids [m]
      (shape: {[m]i64 | \x -> Range x (0,inf)})
      : {([]i64, []bool) | \(ids, flags) -> true } =
  let flags1 = map (\i -> i + 1) (iota m)
  let zero = 0
  let flags = mk_flag_array shape zero flags1
  let flags_sgmind = map (\f -> if f == 0 then 0 else f-1) flags
  let flags_bool = map (\f -> f > 0) flags
  in (sgm_sum flags_bool flags_sgmind, flags_bool)

def valid_neighbour (random_state: []i64) (C: []i64) (state: i64)
                    (neighbour: i64): i64 =
  if (C[neighbour] == 1) && (random_state[neighbour] < state) then
    1
  else
    0

def edges_of_vertex_or_0 (shape: []i64) (newI: []bool)
                         (vert: i64): i64 =
  if !newI[vert] then
    0
  else
    shape[vert]

def sum [n] (xs: [n]i64): {i64 | \_ -> true }=
  if n > 0 then (scan (\x y -> x + y) 0 xs)[n-1] else 0

def slice [n] 't
          (x: [n]t)
          (a: {i64 | \a' -> Range a' (0,inf)})
          (b: {i64 | \b' -> Range b' (0,n+1)}): {[]t | \_ -> true} =
  map (\i -> x[i + a]) (iota (b - a))

def can_add [nVerts] [nEdges]
            (shape: {[nVerts]i64 | \x -> Range x (0,nEdges+1)})
            (offsets: {[nVerts]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x})
            (edges: {[nEdges]i64 | \x -> Range x (0, nVerts)})
            (random_state: [nVerts]i64) (C: [nVerts]i64)
            (index: {i64 | \x -> Range x (0,nVerts)})
            : {bool | \_ -> true} =
  if (C[index] == 0) then
    false
  else
    let vEntry = offsets[index]
    let num = shape[index]
    let end = if vEntry + num < nEdges then vEntry + num else 0
    let currentEdges = slice edges vEntry end

    let arr = map (\x -> valid_neighbour random_state C random_state[index] x)
                  currentEdges

    let valid = sum arr
    in valid == 0

def remove_neighbour_and_self [nVerts] (marked: []i64) (targets: []i64)
                                       (C: *[nVerts]i64): *[nVerts]i64 =
  let zeros1 = map (\_ -> 0) marked
  let zeros2 = map (\_ -> 0) targets
  let C = scatter C targets zeros2
  in scatter C marked zeros1

def mk_sizes [nVerts]
             (nEdges: {i64 | \x -> Range x (0,inf)})
             (shape: {[nVerts]i64 | \x -> Range x (0,nEdges+1)})
             (newI: [nVerts]bool)
             : {[nVerts]i64 | \y -> Range y (0,nEdges+1)} =
  map (\v -> if newI[v] then shape[v] else 0) (iota nVerts)

def mis_step_ [nVerts] [nEdges]
             (shape: {[nVerts]i64 | \x -> Range x (0,nEdges+1) && Equiv nEdges (sum x)})
             -- (offsets: {[nVerts]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x && Equiv x (scan (+) 0 shape)})
             (offsets: {[nVerts]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x})
             (edges: {[nEdges]i64 | \x -> Range x (0, nVerts)})
             (newI: [nVerts]bool)
             (C: *[nVerts]i64) (I: *[nVerts]i64)
             : {(*[nVerts]i64, *[nVerts]i64) | \_ -> true} =
  -- Map the index of each 0-flag to -1, as to be ignored by scatter
  let targets = map2 (\added j -> if added then j else -1) newI (iota nVerts)
  -- Update our MIS with found values
  let newI_i64 = map (\b -> if b then 1 else 0) newI
  let I = scatter I targets newI_i64

  -- For each newly added vertex, get its neighbours
  -- let szs = map (\v -> if newI[v] then shape[v] else 0) (iota nVerts)
  let szs = mk_sizes nEdges shape newI
  let (idxs, flags) = segment_ids szs
  let ones = map (\_ -> 1) flags
  let iotas_p1 = sgm_sum flags ones
  let iotas = map (\i -> i-1) iotas_p1

  let marked_idxs = map2 (\i j -> offsets[i] + j) idxs iotas
  -- let marked = map2 (\i j -> edges[offsets[i] + j]) idxs iotas
  let marked = map (\i -> edges[i]) marked_idxs

  -- Remove the vectors neighbours and self
  let C = remove_neighbour_and_self marked targets C
  in (C, I)

def mis_step [nVerts] [nEdges]
             (shape: {[nVerts]i64 | \x -> Range x (0,nEdges+1)})
             (offsets: {[nVerts]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x})
             (edges: {[nEdges]i64 | \x -> Range x (0, nVerts)})
             (random_state: [nVerts]i64)
             (C: *[nVerts]i64) (I: *[nVerts]i64)
             : {(*[nVerts]i64, *[nVerts]i64) | \_ -> true} =
  -- Get an array of flags for which vertexes can be added to MIS
  let newI = map (\i -> can_add shape offsets edges random_state C i) (iota nVerts)
  in mis_step_ shape offsets edges newI C I

let MIS [nVerts] (nEdges: i64)
                 (shape: {[nVerts]i64 | \x -> Range x (0,nEdges+1)})
                 (edges: {[]i64 | \x -> Range x (0, nVerts)})
                 (random_state: [nVerts]i64) (C: *[nVerts]i64)
                 (I: *[nVerts]i64)
                 : {[]i64 | \_ -> true} =
  -- Reconstruct offsets from shape for O(1) edge access
  let offsets_inclusive = scan (+) 0 shape
  let offsets = map2 (\tot s -> tot - s) offsets_inclusive shape

  -- Loop until every vertex is added to or excluded from the MIS
  let (_, I) = loop (C, I) while (i64.sum C) > 0 do
    mis_step shape offsets edges random_state C I
  in I

-- Input uses the Adjacency graph description:
--   https://cmuparlay.github.io/pbbsbench/fileFormats/graph.html
--
-- Moreover, edges_enc is monotonic within each segment defined by
-- vertexes_enc. (In the prototype compiler, we cannot encode this yet,
-- but the paper would use the `For` property.)
--
-- def main [nVerts] [nEdges] (vertexes_enc: {[nVerts]i32 | \v -> Range v (0,
-- nEdges) && Monotonic (<=) v}) (edges_enc: {[nEdges]i32 | \e -> Range e (0,
-- nVerts)}): {[]i32 | \_ -> true} =
--
-- def main [nVerts] [nEdges] (vertexes_enc: [nVerts]i32)
--                            (edges_enc: [nEdges]i32) =
--   let indexes = iota nVerts

--   -- Random seed, could be anything
--   let s5 = map (\_ -> 5) (iota 1)
--   let s3 = map (\_ -> 3) (iota 1)
--   let s1 = map (\_ -> 1) (iota 1)
--   let s8 = map (\_ -> 8) (iota 1)
--   let s0 = map (\_ -> 0) (iota 1)
--   let seed = s5 ++ s3 ++ s1 ++ s8 ++ s0 ++ s0 ++ s8

--   let rng = minstd_rand.rng_from_seed seed
--   -- Shuffle the indexes, giving each vertex a unique random number
--   let (_, random_state) = shuffle.shuffle rng indexes

--   -- Vertexes no longer needed to be checked
--   let C = replicate nVerts 1
--   -- Vertexes part of the MIS
--   let I = replicate nVerts 0

--   let vertexes_64 = map (\x -> i64.i32 x) vertexes_enc
--   let edges_64 = map (\x -> i64.i32 x) edges_enc

--   let extended_vertexes = vertexes_64 ++ (map (\_ -> nEdges) (iota 1))
--   let shape = map (\i -> extended_vertexes[i+1] - extended_vertexes[i]) indexes

--   let res = MIS nEdges shape edges_64 random_state C I

--   in map (\x -> i32.i64 x) res

-- -- ==
-- -- input @ data/randLocalGraph_JR_10_20000000.in
-- -- output @ data/randLocalGraph_JR_10_20000000.out
