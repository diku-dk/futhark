-- Test that the algebraic simplifier can handle an expression
-- involving mixed 32-bit and 64-bit numbers.
-- ==
-- structure { CmpOp 0 If 0 }

let main(b: bool) =
  let n = 3i64
  let i = if b then 0 else 1
  in i64.i32 i >= 0i64 && i64.i32 i < n
