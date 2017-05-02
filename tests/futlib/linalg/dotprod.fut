-- ==
-- input { [0]   [0] }
-- output { 0 }
-- input { [1,2,3] [4,5,6] }
-- output { 32 }

import "/futlib/linalg"

module i32LinAlg = linalg(i32)

let main(as: [#n]i32, bs: [#n]i32): i32 =
  i32LinAlg.dotprod as bs
