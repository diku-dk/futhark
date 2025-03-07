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


-- Return the edge-id pairs with the smallest edge id
def getSmallestPairs [arraySize] (edges: [arraySize][2]i32) (edgeIds: [arraySize]i32) (nVerts: i64) (nEdges: i64): ([]i32, []i32) =
    -- The length of the flattened arrays
    let arraySizeFlat = arraySize * 2
    
    let edge2Ids = map (\i -> [2*i, 2*i+1]) edgeIds

    let flatE = flatten edges :> [arraySizeFlat]i32
    let flatE2i = flatten edge2Ids :> [arraySizeFlat]i32

    let zippedArray = zip flatE flatE2i

    let verts = map i64.i32 flatE

    let H = hist i32.min (i32.i64 (2*nEdges)) nVerts verts flatE2i
    in filter (\i -> H[i.0] == i.1) zippedArray
       --  |> unzip
        |> map (\i -> (i.0, i.1 / 2)) |> unzip

-- Return the edge if it's ID is the smallest, else return placeholder
def getMMEdges (smallestEdgeId: []i32) (e: [2]i32) (i: i32): ([2]i32, [2]i32) =
    if smallestEdgeId[e[0]] == i && smallestEdgeId[e[1]] == i then (e, [i,i]) else ([-1, -1], [-1, -1])

-- Update the marked vertexes and included edges
def update [arraySize] (edges: [arraySize][2]i32) (edgeIds: [arraySize]i32) (smallestEdgeId: []i32)
                       (markedVerts: *[]bool) (includedEdges: *[]bool): (*[]bool, *[]bool) =
    -- The length of the flattened arrays
    let arraySizeFlat = arraySize*2

    let (e, e2i) = unzip (map2 (getMMEdges smallestEdgeId) edges edgeIds)
    let flatE = flatten e :> [arraySizeFlat]i32
    let flatEi2 = flatten e2i :> [arraySizeFlat]i32

    let trues = replicate arraySizeFlat true

    let markedVerts = scatter markedVerts (map i64.i32 (flatE)) trues
    let includedEdges = scatter includedEdges (map i64.i32 (flatEi2)) trues
    in (markedVerts, includedEdges)

-- Remove the marked edges
def removeMarked [arraySize] (markedVerts: []bool) (edges: [arraySize][2]i32) (edgeIds: [arraySize]i32): ([][2]i32, []i32) = 
    zip edges edgeIds
        |> filter (\(v, _) -> !(markedVerts[v[0]] || markedVerts[v[1]]))
        |> unzip

-- Reset the smallest id of each vertex
def resetsmallestEdgeId (smallestEdgeId: []i32): *[]i32 =
    map (\_ -> i32.highest) smallestEdgeId

def MM [nVerts] [nEdges] (edges: [][2]i32) (edgeIds_all: [nEdges]i64) (markedVerts: *[nVerts]bool)
                         (smallestEdgeId: *[nVerts]i32) (includedEdges: *[nEdges]bool) =
    let edgeIds = map i32.i64 edgeIds_all
    let (_, _, _, _, includedEdges) = loop (edges, edgeIds, markedVerts, smallestEdgeId, includedEdges) while (length edges > 0) do
        let (smallestTargets, smallestValues) = getSmallestPairs edges edgeIds nVerts nEdges

        let smallestEdgeId = scatter smallestEdgeId (map (i64.i32) smallestTargets) smallestValues

        let (markedVerts, includedEdges) = update edges edgeIds smallestEdgeId markedVerts includedEdges

        let (edges, edgeIds) = removeMarked markedVerts edges edgeIds

        let smallestEdgeId = resetsmallestEdgeId smallestEdgeId
        -- I don't get why this copy is needed. I feel like it shouldn't be
        in (edges, edgeIds, copy markedVerts, smallestEdgeId, includedEdges)
    in filter (.1) (zip edgeIds_all includedEdges) |> map (.0)

def main [nEdges] (edges_enc: *[nEdges][2]i32) =
    let nVerts = flatten edges_enc |> i32.maximum |> (+1) |> i64.i32

    let edgeIds = iota nEdges

    let markedVerts = replicate nVerts false
    let smallestEdgeId = replicate nVerts i32.highest

    let includedEdges = replicate nEdges false

    in MM edges_enc edgeIds markedVerts smallestEdgeId includedEdges

-- ==
-- mem_16gb input @ data/randLocalGraph_E_10_20000000.in
-- output @ data/randLocalGraph_E_10_20000000.out
-- mem_16gb input @ data/rMatGraph_E_10_20000000.in
-- output @ data/rMatGraph_E_10_20000000.out
-- input @ data/2Dgrid_E_64000000.in
-- output @ data/2Dgrid_E_64000000.out

