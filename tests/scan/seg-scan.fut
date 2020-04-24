-- ==
-- input { [[[1,2],[3,4],[[4,5],[6,7]]] } auto output
let main (input:[][][]i32) =
    let arr = map (map (scan (+) 0) ) input
    in arr
