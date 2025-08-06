-- ==
-- error: aliases previously returned value
def main () : ([]f64, [][]f64) =
  let e_rows = []
  let arr = copy (e_rows)
  let acc = copy ([1.0])
  in loop (acc, arr) = (acc, arr)
     for i < length arr do
       let arr[i] =
         let y = arr[i]
         let x = acc
         in [2.0]
       -- Error, because 'arr[i]' and 'arr' are aliased, yet the latter
       -- is consumed.
       in (arr[i], arr)
