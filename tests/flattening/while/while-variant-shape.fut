-- ==
-- input { [1i32, 4i32, 8i32, 16i32] }
-- auto output
def main (ns: []i32) : []i32 =
  map (\n ->
         let res =
           loop arr = [1]
           while length arr < i64.i32 n do
             arr ++ arr
         in i32.sum res)
      ns