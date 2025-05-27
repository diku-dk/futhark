-- 
--            Prelude
-- 
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

def filter_by [n] 't (cs: [n]bool) (xs: [n]t) (dummy: t)
    : {(i64, []t) | \(_, ys) -> FiltPart ys xs (\i -> cs[i]) (\_i -> true)} =
  let (new_n, is) = filter_indices cs
  let scratch = replicate new_n dummy
  let xs' = scatter scratch is xs
  in (new_n, xs')

-- 
--            Program
--
def getSmallestPairs_core [arraySize]
    (nVerts: i64)
    (nEdges: i64)
    (flat_edges: {[arraySize*2]i64 | \x -> Range x (0, nVerts)})
    (flat_edge2Ids: {[arraySize*2]i64 | \x -> Injective x})
    : {([]i64, []i64) | \(new_edges, new_edgeIds) ->
         Injective new_edges && Injective new_edgeIds
      }
    =
    let H = hist i64.min (nEdges*2) nVerts flat_edges flat_edge2Ids
    let cs = map2 (\i j -> H[i] == j) flat_edges flat_edge2Ids
    let dummy = 0i64
    let (newSize, ys) = filter_by cs flat_edges dummy
    let (_, zs) = filter_by cs flat_edge2Ids dummy
    in (ys :> [newSize]i64, zs :> [newSize]i64)

-- Transforms edgeIds as follows:
--      0,  1,  2,  3,  4,  5
--      |   |   |   |   |   |
--      0,1 2,3 4,5 6,7 8,9 10,11
def expandIds [n] (edgeIds: {[n]i64 | \x -> Monotonic (<) x}): {[n*2]i64 | \y -> Monotonic (<) y} =
    let ys = map (\i -> map (\j -> 2*i + j) (iota 2)) edgeIds
    in flatten ys
-- WEAKER PRE/POSTCONDITION:
-- def expandIds [n] (edgeIds: {[n]i64 | \x -> Injective x}): {[n][2]i64 | \y -> Injective y} =
--     map (\i -> map (\j -> 2*i + j) (iota 2)) edgeIds
-- ^ could be shown as
--   for i < n . for j < m .
--     | True => j + m * edgeIds(i)
-- creates a gap between each distinct m*edgeIds(i) of size m, which
-- is exactly what j iterates over.


-- Return the edge-id pairs with the smallest edge id
def getSmallestPairs [arraySize]
    (nVerts: i64)
    (nEdges: i64)
    (edges: {[arraySize][2]i64 | \x -> Range x (0, nVerts)})
    (edgeIds: {[arraySize]i64 | \x -> Monotonic (<) x})
    : {([]i64, []i64) | \(new_edges, new_edgeIds) ->
         Injective new_edges && Injective new_edgeIds
      }
    =
    -- Transforms edgeIds as follows:
    --      0,  1,  2,  3,  4,  5
    --      |   |   |   |   |   |
    --      0,1 2,3 4,5 6,7 8,9 10,11
    -- let edge2Ids = map (\i -> map (\j -> 2*i + j) (iota 2)) edgeIds
    let flat_edges = flatten edges
    let flat_edge2Ids = expandIds edgeIds
    let (ys,zs) = getSmallestPairs_core nVerts nEdges flat_edges flat_edge2Ids
    in (ys, zs)

-- Return the edge if its ID is the smallest, else return placeholder
def getMMEdges [nVerts]
    (smallestEdgeId: [nVerts]i64)
    (e: {[2]i64 | \x -> Range x (0, nVerts)})
    (i: i64)
    : {([2]i64, [2]i64) | \_ -> true} =
    if smallestEdgeId[e[0]] == i && smallestEdgeId[e[1]] == i
    then (e, replicate 2 i)
    else (replicate 2 (-1), replicate 2 (-1))

-- Update the marked vertexes and included edges
def update [arraySize] [nVerts] [nEdges]
    (smallestEdgeId: [nVerts]i64)
    (edges: {[arraySize][2]i64 | \x -> Range x (0,nVerts)})
    (edgeIds: [arraySize]i64)
    (markedVerts: *[nVerts]bool)
    (includedEdges: *[nEdges]bool)
    : {(*[nVerts]bool, *[nEdges]bool) | \_ -> true}
    =
    -- The length of the flattened arrays
    let arraySizeFlat = arraySize*2

    let zipped = map2 (\edge id -> getMMEdges smallestEdgeId edge id) edges edgeIds
    let (e, e2i) = unzip zipped
    let flatE = flatten e :> [arraySizeFlat]i64
    let flatEi2 = flatten e2i :> [arraySizeFlat]i64

    let trues = replicate arraySizeFlat true

    let markedVerts = scatter markedVerts flatE trues
    let includedEdges = scatter includedEdges flatEi2 trues
    in (markedVerts, includedEdges)

-- Remove the marked edges
def removeMarked [arraySize] [nVerts]
    (markedVerts: [nVerts]bool)
    (edges: {[arraySize][2]i64 | \x -> Range x (0,nVerts)})
    (edgeIds: {[arraySize]i64 | \x -> Injective x})
    : {([][2]i64, []i64) | \(_, new_edgeIds) ->
          Injective new_edgeIds
      }
    =
    -- zip edges edgeIds
    --     |> filter (\(v, _) -> !(markedVerts[v[0]] || markedVerts[v[1]]))
    --     |> unzip
    let cs = map (\v -> !markedVerts[v[0]] || !markedVerts[v[1]]) edges
    let (new_n, is) = filter_indices cs

    let scratch = replicate new_n (map (\_ -> 0i64) (iota 2))
    let edges' = scatter scratch is edges

    let scratch = replicate new_n 0i64
    let edgeIds' = scatter scratch is edgeIds
    in (edges', edgeIds')

-- Reset the smallest id of each vertex
def resetsmallestEdgeId [n] (_smallestEdgeId: [n]i64): {*[n]i64 | \_ -> true} =
    let i64_highest = 9223372036854775807i64
    in replicate n i64_highest

def loopBody [nEdges] [nVerts]
    (edges: [][2]i64)
    (edgeIds: {[]i64 | \y -> Range edges (0, nVerts) && Monotonic (<) y})
    (markedVerts: *[nVerts]bool)
    (smallestEdgeId: *[nVerts]i64)
    (includedEdges: *[nEdges]bool)
    -- : ({[][2]i64 | \x -> Range x (0, nVerts)},
    --    {[]i64 | \y -> Monotonic (<) y},
    --    *[nVerts]bool,
    --    *[nVerts]i64,
    --    *[nEdges]bool)
    : {([][2]i64, []i64, *[nVerts]bool, *[nVerts]i64, *[nEdges]bool) |
        \(edges', edgeIds', _, _, _) -> Range edges' (0, nVerts) && Monotonic (<) edgeIds'}
    =
    let (smallestTargets, smallestValues) =
      getSmallestPairs nVerts nEdges edges edgeIds
    let smallestEdgeId =
      scatter smallestEdgeId smallestTargets smallestValues
    let (markedVerts, includedEdges) =
      update smallestEdgeId edges edgeIds markedVerts includedEdges
    let (edges, edgeIds) = removeMarked markedVerts edges edgeIds
    let smallestEdgeId = resetsmallestEdgeId smallestEdgeId
    in (edges, edgeIds, markedVerts, smallestEdgeId, includedEdges)

def i64_maximum xs = i64.maximum (flatten xs)

def MM [nEdges] [nVerts]
    (edges: [][2]i64)
    (edgeIds: {[]i64 | \y -> Range edges (0, nVerts) && Monotonic (<) y})
    (markedVerts: *[nVerts]bool)
    (smallestEdgeId: *[nVerts]i64)
    (includedEdges: *[nEdges]bool) =
  let (_, _, _, _, includedEdges) = loop (edges, edgeIds, markedVerts, smallestEdgeId, includedEdges) while (length edges > 0) do
      loopBody edges edgeIds markedVerts smallestEdgeId includedEdges
  -- in filter (.1) (zip edgeIds_all includedEdges) |> map (.0)
  let zipped = zip edgeIds includedEdges
  let dummy = -1i64
  let (n, edgeIds') = filter_by includedEdges edgeIds dummy
  in edgeIds'

def main [nEdges]
    (nVerts: i64)
    (edges_enc: {*[nEdges][2]i64 | \x -> Range x (0, nVerts) && nVerts == i64_maximum x + 1})
    : {[]i64 | \edgeIds' -> Injective edgeIds' }
    =
    let edgeIds = iota nEdges

    let markedVerts = replicate nVerts false
    let smallestEdgeId = replicate nVerts i64.highest

    let includedEdges = replicate nEdges false

    in MM edges_enc edgeIds markedVerts smallestEdgeId includedEdges

-- ==
-- mem_16gb input @ data/randLocalGraph_E_10_20000000.in
-- output @ data/randLocalGraph_E_10_20000000.out
-- mem_16gb input @ data/rMatGraph_E_10_20000000.in
-- output @ data/rMatGraph_E_10_20000000.out
-- input @ data/2Dgrid_E_64000000.in
-- output @ data/2Dgrid_E_64000000.out
