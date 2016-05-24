-- ==
-- input {
--   [[1,2,3],[4,5,6],[7,8,9]]
--   [[4,5,6],[7,8,9],[1,2,3]]
-- }
-- output {
--   [[5, 7, 9], [16, 20, 24], [24, 30, 36]]
-- }
-- structure { Map 3 Scan 1 }
fun [[int]] main([[int]] input1, [[int]] input2) =
  let input = map(fn [(int,int)] ([int] r1, [int] r2) =>
                    zip(r1,r2), zip(input1, input2)) in
  let x = scan(fn [(int,int)] ([(int,int)] a, [(int,int)] b) =>
                 let (a1, a2) = unzip(a) in
                 let (b1, b2) = unzip(b) in
                 map(fn (int,int) ((int,int,int,int) quad) =>
                       let (a1x,a2x,b1x,b2x) = quad in
                       -- FAILS in fusion: let (a1x,b1x,a2x,b2x) = quad ????
                       (a1x+b1x,a2x+b2x),
                    zip(a1,a2,b1,b2)),
               --zip(iota(3), iota(3)), input) in
               zip(replicate(3,0), replicate(3,0)), input) in
  map(fn [int] ([(int,int)] r) =>
        map(+, r),
      x)
