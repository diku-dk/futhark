----------------------------------------------------------------------------
--- The main function `MIS` should be decorated with
---   the precondition that its `indexes` argument is injective.
--- The goal would be to prove that the safety of MIS' scatter, i.e.,
---   that the elements of the `targets` array falling in the range of I,
---   i.e., [0, nVerts) are injective. `targets` is computed as:
---     `let targets = map2 (\i j -> j*i + (-1) * (1-i)) newI indexes`
--- The simple way to accomplish this is to re-write function `can_add`
---   to return a boolean and to rewrite the computation of `targets` as:
---     `let targets = map2 (\i j -> if i then j else -1) newI indexes`
---   Since `indexs` is injective, said injectivity is easy to check.
--- The harder way is to work with the unmodified code, other than
---   adding a postcondition to `can_add` specifying that the result
---   is in range [0,1]. Then to prove the desired injectivity, we can
---   try to specialize the index function of `targets` for the possible
---   values of `i`. This results in:
---     ixfn_{targets} = forall i < n . newI[i] == 1 -> indexes[i]
---                                     newI[i] == 0 -> -1 
---   which can be reasoned the same as before.
---
--- The two other scatter operations appearing in function
---   `remove_neighbour_and_self` are trivial since they exhibit
---   replicated values.
---
--- Same as in the other PBBS benchmark, this seem to use extend
---   from segmented, so we should look at it.
----------------------------------------------------------------------------


def valid_neighbour (random_state: []i64) (C: []i64) (state: i64) (neighbour: i64): i64 =
    if (C[neighbour] == 1) && (random_state[neighbour] < state) then
        1
    else
        0

def edges_of_vertex (verts: []i64) (nEdges: i64) (vert: i64): i64 =
    let extended = verts ++ [nEdges]
    in i64.i64 (extended[vert + 1] - extended[vert])

def edges_of_vertex_or_0 (verts: []i64) (nEdges: i64) (newI: []i64) (vert: i64): i64 =
    if (newI[vert] == 0) then
        0
    else 
        edges_of_vertex verts nEdges vert

def sum [n] (xs: [n]i64): i64 =
  if n > 0 then (scan (\x y -> x + y) 0 xs)[n-1] else 0

def can_add [nVerts] [nEdges] (vertexes: [nVerts]i64) (edges: [nEdges]i64) (random_state: [nVerts]i64) (C: [nVerts]i64) (index: i64): bool =
    C[index] == 0 || (
        let vEntry = (i64.i64 vertexes[index])
        let currentEdges = edges[vEntry:vEntry + (edges_of_vertex vertexes nEdges index)]

        let arr = map (valid_neighbour random_state C random_state[index]) currentEdges

        let valid = sum arr
        in valid == 0)

def mark_neighbour (vertexes: []i64) (edges: []i64) (index: i64) (i: i64): i64 =
    let edgeStartIndex = vertexes[index]
    in (i64.i64 edges[(i64.i64 edgeStartIndex) + i])

def remove_neighbour_and_self [nVerts] (marked: []i64) (targets: []i64) (C: *[nVerts]i64): *[nVerts]i64 =
    let zeros1 = map (\_ -> 0) marked
    let zeros2 = map (\_ -> 0) targets
    let C = scatter C targets zeros2
    in scatter C marked zeros1

def repl_segm_iota = ???

def expand 'a 'b (sz: a -> i64) (get: a -> i64 -> b) (arr:[]a) : []b =
  let szs = map sz arr
  let (idxs, iotas) = repl_segm_iota szs
  in map2 (\i j -> get arr[i] j) idxs iotas

def loop_body [nVerts] (vertexes: [nVerts]i64) (edges: []i64) (random_state: [nVerts]i64) (C: *[nVerts]i64) (I: *[nVerts]i64) (indexes: {[]i64 | \x -> Injective x}) (nEdges: i64) =
  let newI = map (can_add vertexes edges random_state C) indexes
  let targets = map2 (\i j -> if i then j else -1) newI indexes
  let newI_int = map (\i -> if i then 1 else 0) newI
  let I = scatter I targets newI_int

  let marked = expand (edges_of_vertex_or_0 vertexes nEdges newI_int) (mark_neighbour vertexes edges) indexes
  let C = remove_neighbour_and_self marked targets C
  in (C, I)

-- Can probably be done without mapping over every vertex each loop, by keeping track of a queue-like array
let MIS [nVerts] (vertexes: [nVerts]i64) (edges: []i64) (random_state: [nVerts]i64) (C: *[nVerts]i64) (I: *[nVerts]i64) (indexes: {[]i64 | \x -> Injective x}) (nEdges: i64) =
    -- Loop until every vertex is added to or excluded from the MIS
    let (_, I) = loop (C, I) while (i64.sum C) > 0 do
        -- Get an array of flags for which vertexes can be added to MIS
        let newI = map (can_add vertexes edges random_state C) indexes
        -- Map the index of each 0-flag to -1, as to be ignored by scatter
        let targets = map2 (\i j -> if i then j else -1) newI indexes
        -- Update our MIS with found values
        let newI_int = map (\i -> if i then 1 else 0) newI
        let I = scatter I targets newI_int

        -- For each newly added vertex, get its neighbours 
        let marked = expand (edges_of_vertex_or_0 vertexes nEdges newI_int) (mark_neighbour vertexes edges) indexes
        -- Remove the vectors neighbours and self
        let C = remove_neighbour_and_self marked targets C
        in (C, I)
    in I |> (map i64.i64)
