----------------------------------------------------------------------------
--- The main function `MIS` should be decorated with
---   the precondition that its `indexes` argument is injective.
--- The goal would be to prove that the safety of MIS' scatter, i.e.,
---   that the elements of the `targets` array falling in the range of I,
---   i.e., [0, nVerts) are injective. `targets` is computed as:
---     `let targets = map2 (\i j -> if i then j else -1) newI indexes`
---   Since `indexs` is injective, said injectivity is easy to check.
---
--- The two other scatter operations appearing in function
---   `remove_neighbour_and_self` are trivial since they exhibit
---   replicated values.
---
--- Same as in the other PBBS benchmark, this seem to use extend
---   from segmented, so we should look at it.
----------------------------------------------------------------------------


def valid_neighbour [nVerts]
                    (random_state: [nVerts]i64)
                    (C: [nVerts]i64)
                    (state: i64)
                    (neighbour: i64)
                    : i64 =
    if (C[neighbour] == 1) && (random_state[neighbour] < state) then
        1
    else
        0

def sum [n] (xs: [n]i64): {i64 | \_ -> true }=
  if n > 0 then (scan (\x y -> x + y) 0 xs)[n-1] else 0

def slice [n] 't
          (x: [n]t)
          (a: {i64 | \a' -> Range a' (0,inf)})
          (b: {i64 | \b' -> Range b' (0,n+1)}) =
  map (\i -> x[i]) (iota (b - a))

def can_add [nEdges]
            (nVerts: { i64 | \ x -> 0 <= x })
            (vertexes: {[nVerts+1]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x})
            (edges: [nEdges]i64)
            (random_state: [nVerts]i64)
            (C: [nVerts]i64)
            (index: { i64 | \ x -> Range x (0,nVerts-1) })
            : {bool | \_ -> true} =
    C[index] == 0 || (
        let vEntry = vertexes[index]
        let end = vEntry + vertexes[index+1] - vertexes[index]
        let currentEdges = slice edges vEntry end

        let arr = map (\x -> valid_neighbour random_state C random_state[index] x) currentEdges

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

def repl_segm_iota x = (x,x) -- used to be ???

-- pre-conditions:
--   0 <= nInds <= nVerts
--   0 <= newI <= 1
--   0 <= indexes < nVerts
--
def expand [nEdges] [nInds]
           (nVerts: {i64 | \x -> 0 <= x})
           (vertexes: {[nVerts+1]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x})
           (edges: [nEdges]i64)
           (newI: { []i64 | \ x -> Range x (0,1)})
           (indexes: { [nInds]i64 | \ x -> Range x (0,nVerts-1)})
           : {[]i64 | \_ -> true} =
  let szs = map (\ ind -> if (newI[ind] == 0) then 0 else vertexes[ind+1] - vertexes[ind] ) indexes
  -- (unsupported) postcondition should be
  --   0 <= szs[i] <= vertexes[indexes[i]+1] - vertexes[indexes[i]]
  let (idxs, iotas) = repl_segm_iota szs
  -- ^ postconditions of repl_segm_iota:
  --   idxs and iotas are segmented arrays of outer dim nInds
  --   idxs = for i < nInds, j < szs[i]. true => i
  --   iotas= for i < nInds, j < szs[i]. true => j
  --   Hence it should be able to prove that
  --   0 <= iotas[i,j] < vertexes[indexes[i]+1] - vertexes[indexes[i]]
  -- 
  in  map2 (\i j -> edges[ vertexes[ indexes[i] ] + j] ) idxs iotas
  -- IxFn of idxs ensure `0 <= indexes[i] < nInds`, i.e., indexes[i] is in range
  -- From the range post-condition on iotas we have that:
  --     0 <= j < vertexes[indexes[i]+1] - vertexes[indexes[i]]
  -- hence proving index within bounds of `edges` by Fourier Motzkin
  --   eliminates `j`:  vertexes[ indexes[i] ] + vertexes[indexes[i]+1] - vertexes[indexes[i]] - 1 < nEdges
  --   simplification:  vertexes[indexes[i]+1] - 1 < nEdges
  --   eliminate vertexes[indexes[i]+1]:  nEdges - 1 < nEdges hence success.


def loop_body [nEdges]
              (nVerts: { i64 | \ x -> 0 <= x })
              (vertexes: {[nVerts+1]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x})
              (edges: [nEdges]i64)
              (random_state: [nVerts]i64)
              (C: *[nVerts]i64)
              (I: *[nVerts]i64)
              (indexes: {[]i64 | \x -> Injective x})
            : {([]i64, []i64) | \_ -> true} =
  let newI = map (\i -> can_add nVerts vertexes edges random_state C i) indexes
  -- XXX this is what we need to prove inj
  let targets = map2 (\i j -> if i then j else -1) newI indexes
  let newI_int = map (\i -> if i then 1 else 0) newI
  let I = scatter I targets newI_int

  let marked = expand nVerts vertexes edges newI_int indexes
  let C = remove_neighbour_and_self marked targets C
  in (C, I)

let MIS (nVerts: { i64 | \x -> 0 <= x })
        (nEdges: { i64 | \x -> 0 <= x })
        (nInds: {i64 | \x -> Range x (0,nVerts) })
        (vertexes: {[nVerts+1]i64 | \x -> Range x (0,nEdges+1) && Monotonic (<=) x})
        (edges: [nEdges]i64)  -- in principle `Range edges (0, nVerts-1)` but don't think it is used
        (random_state: [nVerts]i64)
        (C: *[nVerts]i64)
        (I: *[nVerts]i64)
        (indexes: {[]i64 | \x -> Range x (0,nVerts-1) && Injective x})
    : { []i64 | \ _ -> true } =
    --
    -- Loop until every vertex is added to or excluded from the MIS
    let (_, I) = loop (C, I) while (i64.sum C) > 0 do
        loop_body nVerts vertexes edges random_state C I indexes
    in I
 
-- verify with:
-- FUTHARK_INDEXFN=1 cabal test --test-option="--pattern=Properties.IndexFn.mis"
