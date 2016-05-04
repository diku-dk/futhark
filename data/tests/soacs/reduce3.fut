-- This test checks whether empty reduces are handled properly.
-- ==
-- input {
--   0
-- }
-- output {
--   False
--   0
-- }
fun (bool,int) main(int n) =
  let (a,b) = reduce(fn (bool,int) ((bool,int) acc, (bool,int) elem) =>
                       let (accx, accy) = acc in
                       let (x, y) = elem in
                       (accx && x,
                        y),
                     (False,0), zip(replicate(n,True), replicate(n,1))) in
  (a,b)
