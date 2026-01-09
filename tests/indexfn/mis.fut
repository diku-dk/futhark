-- import "lib/github.com/diku-dk/cpprandom/random"
-- import "lib/github.com/diku-dk/cpprandom/shuffle"

-- module shuffle = mk_shuffle minstd_rand

-- 
-- Prelude.
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

def sum [n] (xs: [n]i64): {i64 | \_ -> true}=
  if n > 0 then (scan (\x y -> x + y) 0 xs)[n-1] else 0

def slice [n] 't
    (x: [n]t)
    (a: {i64 | \a' -> Range a' (0,inf)})
    (b: {i64 | \b' -> Range b' (0,n+1)})
    : {[]t | \_ -> true} =
  map (\i -> x[i + a]) (iota (b - a))

-- 
-- Program.
-- 
def valid_neighbour
    (random_state: []i64)
    (C: []i64)
    (state: i64)
    (neighbour: i64)
    : i64 =
  if (C[neighbour] == 1) && (random_state[neighbour] < state) then
    1
  else
    0

def can_add [V] [E]
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (edges: {[E]i64 | \x -> Range x (0, V)})
    (random_state: [V]i64) (C: [V]i64)
    (index: {i64 | \x -> Range x (0,V)})
    : {bool | \_ -> true} =
  if (C[index] == 0) then
    false
  else
    let start = offsets[index]
    let end = offsets[index+1]
    let currentEdges = slice edges start end
    let arr = map (\x ->
      valid_neighbour random_state C random_state[index] x
    ) currentEdges
    let valid = sum arr
    in valid == 0

def remove_neighbour_and_self [V]
    (marked: []i64)
    (targets: []i64)
    (C: *[V]i64)
    : *[V]i64 =
  let zeros1 = map (\_ -> 0) marked
  let zeros2 = map (\_ -> 0) targets
  let C = scatter C targets zeros2
  in scatter C marked zeros1

def make_shape [V]
    (E: i64)
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (new: [V]bool)
    : {[V]i64 | \y -> For y (\i -> Range y (0, offsets[i+1] - offsets[i]))} =
  map (\i -> if new[i] then offsets[i+1] - offsets[i] else 0) (iota V)

def expand_indices [V]
    (E: i64)
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (new: [V]bool)
    : {([]i64, []i64) | \(seg,y) -> For y (\i -> Range y (0, offsets[seg[i]+1]))} =
  let new_shape = make_shape E offsets new
  let (segment_ids, iotas) = repl_segm_iota new_shape
  in (segment_ids, map2 (\i j -> offsets[i] + j) segment_ids iotas)

def expand [V] [E]
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (edges: {[E]i64 | \x -> Range x (0, V)})
    (new: [V]bool)
    : {[]i64 | \_ -> true} =
  -- For each newly added vertex, get its neighbours
  let (_, indices) = expand_indices E offsets new
  in map (\i -> edges[i]) indices

def mis_step_ [V] [E]
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (edges: {[E]i64 | \x -> Range x (0, V)})
    (new: [V]bool)
    (C: *[V]i64)
    (I: *[V]i64)
    : {(*[V]i64, *[V]i64) | \_ -> true} =
  -- Update the MIS
  let targets = map2 (\added j -> if added then j else -1) new (iota V)
  let new_i64 = map (\b -> if b then 1 else 0) new
  let I = scatter I targets new_i64

  -- Remove the vectors neighbours and self
  let marked = expand offsets edges new
  let C = remove_neighbour_and_self marked targets C
  in (C, I)

def mis_step [V] [E]
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (edges: {[E]i64 | \x -> Range x (0, V)})
    (random_state: [V]i64)
    (C: *[V]i64)
    (I: *[V]i64)
    : {(*[V]i64, *[V]i64) | \_ -> true} =
  -- Update the MIS
  let new = map (\i -> can_add offsets edges random_state C i) (iota V)
  in mis_step_ offsets edges new C I


def MIS [V] [E]
    (offsets: {[V+1]i64 | \x -> Range x (0,E) && Monotonic (<=) x})
    (edges: {[E]i64 | \x -> Range x (0, V)})
    (random_state: [V]i64)
    (C: *[V]i64)
    (I: *[V]i64)
    : {[]i64 | \_ -> true} =
  -- Loop until every vertex is added to or excluded from the MIS
  let (_, I) = loop (C, I) while (i64.sum C) > 0 do
    mis_step offsets edges random_state C I
  in I
