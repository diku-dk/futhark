-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
--   [[4,5,6],[7,8,9],[1,2,3]]
-- }
-- output {
--   [[5, 7, 9], [16, 20, 24], [24, 30, 36]]
-- }
-- structure { Map 3 Scan 1 }
fun main(input1: [][]int, input2: [][]int): [][]int =
  let input = map(fn (r1: []int, r2: []int): [](int,int)  =>
                    zip(r1,r2), zip(input1, input2)) in
  let x = scan(fn (a: [](int,int), b: [](int,int)): [](int,int)  =>
                 let (a1, a2) = unzip(a) in
                 let (b1, b2) = unzip(b) in
                 map(fn (quad: (int,int,int,int)): (int,int)  =>
                       let (a1x,a2x,b1x,b2x) = quad in
                       -- FAILS in fusion: let (a1x,b1x,a2x,b2x) = quad ????
                       (a1x+b1x,a2x+b2x),
                    zip(a1,a2,b1,b2)),
               --zip(iota(3), iota(3)), input) in
               zip(replicate(3,0), replicate(3,0)), input) in
  map(fn (r: [](int,int)): []int  =>
        map(+, r),
      x)
