-- ==
-- input {
--   [1,2,3,4,5,6,7]
-- }
-- output {
--   [-1, -1, 0, 2, 5, 9, 14]
-- }
fun [int] main([int] a) =
  let (_,b) = unzip(map(fn (int,int) (int x) => (x+2,x-2), a)) in
  let c = scan(+, 0, b) in
  c
