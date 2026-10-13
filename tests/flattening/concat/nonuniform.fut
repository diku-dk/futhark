-- If we have a nonuniform concat inside a loop, that really has to be
-- flattened.
-- ==
-- input { [1i64, 2i64, 3i64] 3i64 }
-- auto output
-- structure gpu { /Loop 1 /Loop/Loop 1 }

def main (ns: []i64) (k: i64) : []i64 =
  map (\n ->
         loop acc = 0
         for _i < k do
           let res = loop arr = [1i64] while length arr < n do arr ++ arr
           in acc + res[0])
      ns
