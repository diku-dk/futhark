-- An unflatten moved past a later flatten must keep its own input
-- width.
-- ==
-- input { [[[[0],[1]],[[2],[3]]],[[[4],[5]],[[6],[7]]],[[[8],[9]],[[10],[11]]],[[[12],[13]],[[14],[15]]]] }
-- output { [[[[0],[1],[2],[3]],[[4],[5],[6],[7]]],[[[8],[9],[10],[11]],[[12],[13],[14],[15]]]] }

entry main (xs: [4][2][2][1]i32) =
  map (map flatten) (unflatten (xs :> [2 * 2][2][2][1]i32))
