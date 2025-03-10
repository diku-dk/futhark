-- Create edge-id pairs from edge ids.
def flattenEdge2Ids [n] (edgeIds: {[n]i64 | \x -> InjectiveRCD (0, n-1) x})
    : { []i64 | \ys -> InjectiveRCD (0, length ys - 1) ys } =
  let edge2Ids = map (\i -> [2*i, 2*i+1]) edgeIds
  in flatten edge2Ids
