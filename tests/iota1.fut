-- Iota with some different types.
-- ==
-- input { 2 }
-- output { [0i8,1i8] [0i16,1i16] [0i32,1i32] [0i64,1i64]
--        }

import "/futlib/math"

let main(n: i32): ([]i8, []i16, []i32, []i64) =
  (i8.iota (i8 n), i16.iota (i16 n), i32.iota (i32 n), i64.iota (i64 n))
