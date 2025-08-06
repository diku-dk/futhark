-- ==
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 3 }

def main [n] (ind: i64) (as: [n]i64) =
  let tmp1 = map (* 2) as |> opaque
  let tmp2 = map (* 3) as |> opaque
  let tmp3 = map (* 4) as |> opaque
  let tmp = concat tmp2 tmp3
  let res = concat tmp1 tmp
  in res
