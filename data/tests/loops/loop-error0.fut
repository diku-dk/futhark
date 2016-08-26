-- ==
-- error:
fun main(): ([]f64,[][]f64) =
  let e_rows = empty([]f64) in
  let arr = copy(e_rows) in
  let acc = copy([1.0]) in
  loop ((acc, arr) = (acc, arr)) = for i < size(0, arr) do
    let arr[i] = let y = arr[i] in
                 let x = acc in
                 [2.0] in
    -- Error, because 'arr[i]' and 'arr' are aliased, yet the latter
    -- is consumed.
    (arr[i], arr) in
  (acc,arr)
