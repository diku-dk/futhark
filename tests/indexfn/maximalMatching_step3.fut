----------------------------------------------------------------------------------------------
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
---   operation to create the following query that if solved proves injectivity:
---     for all i1, i2 integers in [0..arraySize) satisfying the properties
---          flatE[i1] == flatE[i2] &&
---          H[flatE[i1]] == flatE2[i1] &&  -- necessary for index i1 to be kept by filter
---          H[flatE[i2]] == flatE2[i2] &&  -- necessary for index i2 to be kept by filter
---       => i1 = i2
---   The proof can be accomplished by a technique aimed at deriving new equalities, e.g.,
---     flatE[i1] == flatE[i2] == z (fresh) => 
---     H[z] == flatE2[i1] && H[z] == flatE2[i2] =>
---     flatE2[i1] == flatE2[i2] => i1 == i2 (because `flatE2` was proved injective)
----------------------------------------------------------------------------------------------

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
      let split = sum (map (\x -> to_i64 x) cs)
      in FiltPartInv is (\i -> cs[i]) (\i -> true) split
    } =
  let num_trues = scan (+) 0 (map (\c -> to_i64 c) cs)
  let new_size = if n > 0 then num_trues[n-1] else 0
  let is = map2 (\c i -> if c then i-1 else -1) cs num_trues
  in (new_size, is)

def filter [n] 't (p: t -> bool) (xs: [n]t) : {[]t | \_ -> true} =
  let cs = map (\x -> p x) xs
  let (new_n, is) = filter_indices cs
  let dummy = xs[0]
  let scratch = replicate new_n dummy
  in scatter scratch is xs
--------------------------------------------------------------------------------


-- Create edge-id pairs from edge ids.
def flattenEdge2Ids [n] (edgeIds: {[n]i64 | InjectiveRCD (0, n-1)})
    : { []i64 | \ys -> InjectiveRCD (0, length ys - 1) ys } =
  let edge2Ids = map (\i -> [2*i, 2*i+1]) edgeIds
  in flatten edge2Ids

-- Return the edge-id pairs with the smallest edge id
def getSmallestPairs [n]
      (edges: [n][2]i64)
      (edgeIds: {[n]i64 | InjectiveRCD (0, n-1)})
      (nVerts: i64)
      (nEdges: i64)
      : { ([]i64, []i64) | \(ys, _) -> InjectiveRCD (0, length ys - 1) ys } =
    let flatE = flatten edges
    let flatE2i = flattenEdge2Ids edgeIds

    let verts = flatE
    let H = hist i64.min (2*nEdges) nVerts verts flatE2i

    let zippedArray = zip flatE flatE2i
    let filteredArray = filter (\(i, j) -> H[i] == j) zippedArray
    let (ys, zs) = unzip filteredArray
    let zs' = map (\i -> i / 2) zs
    in (ys, zs')



-- NOTES
-- 
-- [ ] Make flattenEdge2Ids work
-- [ ] Make the index function for getSmallestPairs
-- [ ] Make property env so that we don't forget what we've proved
-- [ ] Create rule for showing injective across filter
--
--
-- 
-- X[i1] = x[i2] => i1 = i2
-- What I know is that x = filter p a
-- lets take i1 and i2 indices of a such that p(a[i1]) == p(a[i2])
-- p(a[i1]) holds and p(a[i2]) holds
-- a[i1] == a[i2] => i1 = i2
-- given that p(a[i1]) holds and p(a[i2]) holds
-- p(a[i1])&& p(a[i2]) && a[i1] = a[i2] => i1 = i2
