
def szs [nVerts]
     (vertexes: {[nVerts+1]i64 | \x -> Monotonic (<=) x})
     : {[nVerts]i64 | \_ -> true} =
  map (\ ind -> vertexes[ind+1] - vertexes[ind] ) (iota nVerts)

