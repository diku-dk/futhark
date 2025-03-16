--------------------------------------------------------------------------------
-- 
-- PRELUDE
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

def filterBy [n] 't (cs: [n]bool) (xs: [n]t) : {(i64, []t) | \_ -> true} =
  let (new_n, is) = filter_indices cs
  let dummy = xs[0]
  let scratch = replicate new_n dummy
  in (new_n, scatter scratch is xs)

-- def filter [n] 't (p: t -> bool) (xs: [n]t) : {[]t | \_ -> true} =
--   let cs = map (\x -> p x) xs
--   let (new_n, is) = filter_indices cs
--   let dummy = xs[0]
--   let scratch = replicate new_n dummy
--   in scatter scratch is xs
--------------------------------------------------------------------------------

-- maximalMatching from the PBBS.
-- https://github.com/cmuparlay/pbbsbench
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
---       for all i, j integers in [0..arraySize) satisfying the properties
---          flatE[i] == flatE[j] &&
---          H[flatE[i]] == flatE2[i] &&  -- necessary for index i to be kept by filter
---          H[flatE[j]] == flatE2[j] &&  -- necessary for index j to be kept by filter
---       => i = j
---   The proof can be accomplished by a technique aimed at deriving new equalities, e.g.,
---     flatE[i] == flatE[j] == z (fresh) => 
---     H[z] == flatE2[i] && H[z] == flatE2[j] =>
---     flatE2[i] == flatE2[j] => i == j (because `flatE2` was proved injective)
----------------------------------------------------------------------------------------------

def edge2Ids_helper [n] nEdges (xs: {[n]i64 | \xs' -> Monotonic (<)  xs'})
    : { [n]i64 | \ys -> Monotonic (<) ys } =
  map (\i -> 2*i) xs

def edge2Ids_helpe2r [n] nEdges (xs: {[n]i64 | \xs' -> InjectiveRCD (-infty, infty)  xs'})
    : { [n]i64 | \ys -> Monotonic (<) ys } =
  map (\i -> 2*i) xs

Injective xs

-- Return the edge-id pairs with the smallest edge id
def getSmallestPairs [arraySize] (nEdges: i64) (edges: [arraySize]i64) (edgeIds: {[arraySize]i64 | \x -> Monotonic (<) x}) (nVerts: i64) 
      : {([]i64, []i64) | \(ys,is) -> InjectiveRCD (0,length ys - 1) ys && Monotonic (<) is} =
    -- NOTE We don't support multi-dim yet, so I removed one dimension.
    -- let edge2Ids = map (\i -> 2*i) edgeIds
    let edge2Ids = edge2Ids_helper edgeIds

    let flatE = edges
    let flatE2i = edge2Ids

    let zippedArray = zip flatE flatE2i

    let verts = flatE

    let H = hist i64.min (2*nEdges) nVerts verts flatE2i
    -- let filteredArray = filter (\(i, j) -> H[i] == j) zippedArray
    -- let (ys, zs) = unzip filteredArray
    -- let zs' = map (\i -> i / 2) zs
    --
    -- NOTE I rewrite the above lines to be compatible with the language we support.
    -- Supporting the above is simple; change zipArgs to apply the function
    -- for each arg that turns out to be tupled.
    -- The division can be treated as an uninterpreted function, or by exploiting
    -- that the division simply cancels out the factor: i/2 = 2*edgeIds[j]/2 = edgeIds[j].
    let cs = map (\(i, j) -> H[i] == j) zippedArray
    let (newSize, ys) = filterBy cs flatE
    let (_, zs) = filterBy cs flatE2i
    let zs' = zs
    let hm = filter_indices cs
    in (ys :> [newSize]i64, zs' :> [newSize]i64)

-- Return the edge if it's ID is the smallest, else return placeholder
def getMMEdges (smallestEdgeId: []i64) (e: i64) (i: i64): (i64, i64) =
    if smallestEdgeId[e] == i then (e, i) else (-1, -1)

-- Update the marked vertexes and included edges
def update [arraySize] (edges: [arraySize]i64) (edgeIds: [arraySize]i64) (smallestEdgeId: []i64)
                       (markedVerts: *[]bool) (includedEdges: *[]bool): (*[]bool, *[]bool) =

    let (e, e2i) = unzip (map2 (getMMEdges smallestEdgeId) edges edgeIds)
    let flatE = e
    let flatEi2 = e2i

    let trues = replicate arraySize true

    let markedVerts = scatter markedVerts flatE trues
    let includedEdges = scatter includedEdges flatEi2 trues
    in (markedVerts, includedEdges)

-- Remove the marked edges
def removeMarked [arraySize] (markedVerts: []bool) (edges: [arraySize]i64) (edgeIds: [arraySize]i64): ([]i64, []i64) = 
    zip edges edgeIds
        |> filter (\(v, _) -> !markedVerts[v])
        |> unzip

-- Reset the smallest id of each vertex
def resetsmallestEdgeId (smallestEdgeId: []i64): *[]i64 =
    map (\_ -> i64.highest) smallestEdgeId

def MM [nVerts] [nEdges] (edges: []i64) (edgeIds_all: [nEdges]i64) (markedVerts: *[nVerts]bool)
                         (smallestEdgeId: *[nVerts]i64) (includedEdges: *[nEdges]bool) =
    let edgeIds = edgeIds_all
    let (_, _, _, _, includedEdges) = loop (edges, edgeIds, markedVerts, smallestEdgeId, includedEdges) while (length edges > 0) do
        let (smallestTargets, smallestValues) = getSmallestPairs edges edgeIds nVerts nEdges

        let smallestEdgeId = scatter smallestEdgeId smallestTargets smallestValues

        let (markedVerts, includedEdges) = update edges edgeIds smallestEdgeId markedVerts includedEdges

        let (edges, edgeIds) = removeMarked markedVerts edges edgeIds

        let smallestEdgeId = resetsmallestEdgeId smallestEdgeId
        -- I don't get why this copy is needed. I feel like it shouldn't be
        in (edges, edgeIds, copy markedVerts, smallestEdgeId, includedEdges)
    in filter (.1) (zip edgeIds_all includedEdges) |> map (.0)

def main [nEdges] (edges_enc: *[nEdges]i64) =
    let nVerts = edges_enc |> i64.maximum |> (+1)

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

