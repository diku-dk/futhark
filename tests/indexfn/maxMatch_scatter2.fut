-- maximalMatching from the PBBS.
-- https://github.com/cmuparlay/pbbsbench
-- 
-- SIMPLIFIED VERSION:
--   * We expand edgeIds so that it is an array [nEdges][2]
--     and let nEdges_2 = nEdges*2.
--     (The inner dimension was computed just in time before.)
--   * We flatten edges and edgeIds.
--   * We rewrite the program so that is restricted to the source language
--     presented in the paper.
-- 
--
----------------------------------------------------------------------------------------------
--- Nice benchmark demonstrating verification
---   of the injectivity property of an array:
---
--- (1) The first goal (function MM):
---   given the precondition that argument `edgeIds_all` is injective
---   we need to prove that the `edgeIds` loop-variant
---   argument of the loop (implementing `MM`) is injective.
---   This is relatively straightforward given that
---   we have the right constructors in the language
---   of properties, since initially `edgeIds` result of an
---   iteration is obtained by filtering the `edgeIds` input
---   of an iteration, hence injectivity should be preserved.
---
--- (2) The second goal is to prove that the safety of the 3 scatters
---   appearing in the code. The two scatters appearing in function
---   `update` are trivial since they have replicated values
---   (easy to check)
---
--- (3) The scatter appearing in MM's loop is the tricky one: it requires
---   proving that the first array result of function `getSmallestPairs`
---   is injective, i.e., given the precondition that the `edgeIds`
---   argument of function `getSmallestPairs` is injective, prove the
---   postcondition that the first array result of `getSmallestPairs` is
---   injective.
---   This can be tackled by proving that `flatE2i` is injective by
---   solving queries of the form:
---       forall i1 < i2, i1 and i2 being elements of edgeIds =>
---         2*i1 != 2*i2 && 2*i1+1 != 2*i2+1 && 2*i1 != 2*i1+1 && 2*i1+1 != 2*i2
---   Then we can exploit that the first result is obtained by a filter
---     operation to create the following query that if solved proves injectivity:
---       for all i1, i2 integers in [0..arraySize) satisfying the properties
---          flatE[i1] == flatE[i2] &&
---          H[flatE[i1]] == flatE2[i1] &&  -- necessary for index i1 to be kept by filter
---          H[flatE[i2]] == flatE2[i2] &&  -- necessary for index i2 to be kept by filter
---       => i1 = i2
---   The proof can be accomplished by a technique aimed at deriving new equalities, e.g.,
---     flatE[i1] == flatE[i2] == z (fresh) => 
---     H[z] == flatE2[i1] && H[z] == flatE2[i2] =>
---     flatE2[i1] == flatE2[i2] => i1 == i2 (because `flatE2` was proved injective)
----------------------------------------------------------------------------------------------

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

-- 
--            Program
--
type nat64 = {i64 | (>= 0)}

-- Return the edge-id pairs with the smallest edge id
def getSmallestPairs [arraySizeFlat]
    (nVerts: i64)
    (nEdges_2: i64)
    (edges: {[arraySizeFlat]i64 | \x -> Range x (0, nVerts)})
    (edgeIds: {[arraySizeFlat]i64 | \x -> Injective x})
    : {([arraySizeFlat]bool, []i64, []i64) | \(cs, new_edges, new_edgeIds) ->
         FiltPart new_edges edges (\i -> cs[i]) (\_i -> true)
           && Injective new_edges && Injective new_edgeIds
      }
    =
    -- The original program transforms edgeIds as follows:
    --      0,  1,  2,  3,  4,  5
    --      |   |   |   |   |   |
    --      0,1 2,3 4,5 6,7 8,9 10,11
    -- But this is already the form of the pre-expanded edgeIds here.
    -- 
    -- The length of the flattened arrays
    -- let arraySizeFlat = arraySize * 2
    -- let edge2Ids = map (\i -> [2*i, 2*i+1]) edgeIds
    -- let flatE = flatten edges :> [arraySizeFlat]i32
    -- let flatE2i = flatten edge2Ids :> [arraySizeFlat]i32
    -- let zippedArray = zip edges edgeIds
    -- let verts = map i64.i32 flatE

    let H = hist i64.min nEdges_2 nVerts edges edgeIds
    let cs = map2 (\i j -> H[i] == j) edges edgeIds
    let (newSize, inv_perm) = filter_indices cs
    let ys = scatter (replicate newSize 0) inv_perm edges
    let zs = scatter (replicate newSize 0) inv_perm edgeIds
    in (cs, ys :> [newSize]i64, zs :> [newSize]i64)

-- Return the edge if its ID is the smallest, else return placeholder
def getMMEdges [nVerts] (smallestEdgeId: [nVerts]i64) (e: {i64 | \x -> 0 <= x}) (i: i64): (i64, i64) =
    if smallestEdgeId[e] == i then (e, i) else (-1, -1)

-- Update the marked vertexes and included edges
def update [arraySizeFlat] [nVerts]
    (smallestEdgeId: [nVerts]i64)
    (edges: {[arraySizeFlat]i64 | \x -> Range x (0,nVerts)})
    (edgeIds: [arraySizeFlat]i64)
    (markedVerts: *[]bool)
    (includedEdges: *[]bool)
    : {(*[]bool, *[]bool) | \_ -> true}
    =
    -- The length of the flattened arrays
    -- let arraySizeFlat = arraySize*2

    let zipped = map2 (\edge id -> getMMEdges smallestEdgeId edge id) edges edgeIds
    let (e, e2i) = unzip zipped
    -- let flatE = flatten e :> [arraySizeFlat]i64
    -- let flatEi2 = flatten e2i :> [arraySizeFlat]i64

    let trues = replicate arraySizeFlat true

    let markedVerts = scatter markedVerts e trues
    let includedEdges = scatter includedEdges e2i trues
    in (markedVerts, includedEdges)

-- Remove the marked edges
def removeMarked [arraySizeFlat] [nVerts]
    (markedVerts: [nVerts]bool)
    (edges: {[arraySizeFlat]i64 | \x -> Range x (0,nVerts)})
    (edgeIds: {[arraySizeFlat]i64 | \x -> Injective x})
    : {([]i64, []i64) | \(_, new_edgeIds) ->
          Injective new_edgeIds
      }
    =
    -- zip edges edgeIds
    --     |> filter (\(v, _) -> !(markedVerts[v[0]] || markedVerts[v[1]]))
    --     |> unzip
    let cs = map (\v -> !markedVerts[v]) edges
    let (new_n, is) = filter_indices cs

    let scratch = replicate new_n 0i64
    let edges' = scatter scratch is edges

    let scratch = replicate new_n 0i64
    let edgeIds' = scatter scratch is edgeIds
    in (edges', edgeIds')

-- Reset the smallest id of each vertex
def resetsmallestEdgeId [n] (_smallestEdgeId: [n]i64): {*[n]i64 | \_ -> true} =
    let i64_highest = 9223372036854775807i64
    in replicate n i64_highest

def loopBody [arraySizeFlat] [nVerts]
    (edges: {[arraySizeFlat]i64 | \x -> Range x (0,nVerts)})
    (edgeIds: {[arraySizeFlat]i64 | \x -> Injective x})
    (markedVerts: *[nVerts]bool)
    (smallestEdgeId: *[nVerts]i64)
    (includedEdges: *[arraySizeFlat]bool)
    : { ([]i64, []i64, *[]bool, *[nVerts]i64, *[]bool) |
          \(_, new_edgeIds, _, _, _) ->
            Injective new_edgeIds
      }
    =
    let (_, smallestTargets, smallestValues) = getSmallestPairs nVerts arraySizeFlat edges edgeIds

    let smallestEdgeId = scatter smallestEdgeId smallestTargets smallestValues

    let (markedVerts, includedEdges) = update smallestEdgeId edges edgeIds markedVerts includedEdges

    let (edges, edgeIds) = removeMarked markedVerts edges edgeIds

    let smallestEdgeId = resetsmallestEdgeId smallestEdgeId
    in (edges, edgeIds, markedVerts, smallestEdgeId, includedEdges)

def i64_maximum xs = i64.maximum xs

def main [nEdges_2]
    -- (edges_enc: *[nEdges][2]i64)
    (nVerts: i64)
    (edges: {*[nEdges_2]i64 | \x -> Range x (0, nVerts) && nVerts == i64_maximum x + 1})
    : {[]i64 | \edgeIds' -> Injective edgeIds' }
    =
    -- let edges = flatten edges_enc
    -- let max_edge = i64.maximum edges
    -- let nVerts = max_edge + 1

    let edgeIds = iota nEdges_2

    let markedVerts = replicate nVerts false
    let smallestEdgeId = replicate nVerts i64.highest

    let includedEdges = replicate nEdges_2 false

    -- Skipping loop; pre- and postconditions on each loop iteration
    -- is shown by loopBody.
    let (_, edgeIds', _, _, _) = loopBody edges edgeIds markedVerts smallestEdgeId includedEdges
    -- NOTE this is _one_ iteration of the loop, because I removed the loop construct.
    -- Hence the index function resulting below is not correct if the loop was there.
    in edgeIds'

-- def MM [nVerts] [nEdges_2] (edges: [nEdges_2]i64) (edgeIds_all: [nEdges_2]i64) (markedVerts: *[nVerts]bool)
--                          (smallestEdgeId: *[nVerts]i64) (includedEdges: *[nEdges_2]bool) =
--     let edgeIds = edgeIds_all
--     let (_, _, _, _, includedEdges) = loop (edges, edgeIds, markedVerts, smallestEdgeId, includedEdges) while (length edges > 0) do
--         let loopres = loopBody edges edgeIds markedVerts smallestEdgeId includedEdges
--         in loopres
--     in filter (.1) (zip edgeIds_all includedEdges) |> map (.0)

-- def main [nEdges] (edges_enc: *[nEdges][2]i64) =
--     let edges = flatten edges_enc
--     let nVerts = edges |> i64.maximum |> (+1)

--     let edgeIds = iota (nEdges*2)

--     let markedVerts = replicate nVerts false
--     let smallestEdgeId = replicate nVerts i64.highest

--     let includedEdges = replicate (nEdges*2) false

--     in MM edges edgeIds markedVerts smallestEdgeId includedEdges

-- ==
-- mem_16gb input @ data/randLocalGraph_E_10_20000000.in
-- output @ data/randLocalGraph_E_10_20000000.out
-- mem_16gb input @ data/rMatGraph_E_10_20000000.in
-- output @ data/rMatGraph_E_10_20000000.out
-- input @ data/2Dgrid_E_64000000.in
-- output @ data/2Dgrid_E_64000000.out
