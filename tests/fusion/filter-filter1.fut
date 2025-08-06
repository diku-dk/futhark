-- ==
-- input {
--   [1,2,3,4,5,6,7,8,9]
--   [10,20,30,40,50,60,70,80,90]
-- }
-- output {
--   [20, 30, 40, 60, 80, 90]
-- }
def div2 (x: i32) : bool = x % 2 == 0

def div3 (x: i32) : bool = x % 3 == 0

def main (a: []i32) (b: []i32) : []i32 =
  let (c1, c2) =
    unzip (filter (\(x: i32, y: i32) : bool ->
                     div2 (x) || div3 (y))
                  (zip a b))
  in filter div2 c2
