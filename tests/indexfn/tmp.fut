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

