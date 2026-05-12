def reduce_by_index_stream [k] 'a 'b
                           (dest: *[k]a) (f: *[k]a -> b -> *[k]a) : *[k]a =
  dest

def pairInteraction [n] 'a 'b
                    (ne: b)
                    (add: b -> b -> b)
                    (potential: a -> a -> b)
                    (coordinates: [n]a) =
  let interaction [k] (acc: *[k]b) (i: i64, j: i64): *([k]b) =
    let cI = coordinates[i]
    let cJ = coordinates[j]
    let v = potential cI cJ
    in acc
  in reduce_by_index_stream (replicate n ne) interaction
