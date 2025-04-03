-- ==
-- input {
--   [[[1,10,100],[2,20,200],[3,30,300]],[[4,40,400],[5,50,500],[6,60,600]]]
-- }
-- output {
--   [[[1, 2, 3], [10, 20, 30], [100, 200, 300]], [[4, 5, 6], [40, 50, 60], [400,
--                                                                           500,
--                                                                           600]]]
-- }
-- compiled random input { [65537][16][16]i32 } auto output
-- compiled random input { [65537][2][2]i32 } auto output
-- compiled random input { [65537][128][2]i32 } auto output
-- compiled random input { [65537][2][128]i32 } auto output

def main (a: [][][]i32) : [][][]i32 =
  map transpose a
