-- Fusing into a for-in loop.
-- ==
-- input { [[1,2],[3,4],[5,6]] }
-- output { 21 }
-- structure { Stream 1 }

let main (xss: [][]i32) =
  let ys = map (\xs -> reduce (+) 0 xs) xss
  in loop (a = 0) for y in ys do a + y
