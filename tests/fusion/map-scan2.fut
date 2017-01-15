-- ==
-- input {
--   [1,2,3,4,5,6,7]
-- }
-- output {
--   [-1, -1, 0, 2, 5, 9, 14]
-- }
-- structure {
--   Scanomap 1
-- }
fun main(a: []int): []int =
  let (_,b) = unzip(map (\(x: int): (int,int)  -> (x+2,x-2)) a)
  let c = scan (+) 0 b in
  c
