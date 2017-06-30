-- For-in over 2D array.
-- ==
-- input { [[1],[2],[3]] }
-- output { 6 }

let main (xss: [][]i32) =
  loop (a = 0) for xs in xss do a + xs[0]
