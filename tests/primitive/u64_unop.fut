-- Test unary operators for u64.
--
-- ==
-- input { 0  0u64 } output {  0u64 }
-- input { 0  1u64 } output { 18446744073709551615u64 }
-- input { 0 18446744073709551615u64 } output {  1u64 }
-- input { 0  8u64 } output { 18446744073709551608u64 }
-- input { 0 18446744073709551608u64 } output {  8u64 }
--
-- input { 1  0u64 } output { 0u64 }
-- input { 1  1u64 } output { 1u64 }
-- input { 1 18446744073709551615u64 } output { 18446744073709551615u64 }
-- input { 1  8u64 } output { 8u64 }
-- input { 1 18446744073709551608u64 } output { 18446744073709551608u64 }
--
-- input { 2  0u64 } output { 0u64 }
-- input { 2  1u64 } output { 1u64 }
-- input { 2 18446744073709551615u64 } output { 1u64 }
-- input { 2  8u64 } output { 1u64 }
-- input { 2 18446744073709551608u64 } output { 1u64 }


let main (f: i32) (x: u64): u64 =
  if      f == 0 then -x
  else if f == 1 then u64.abs(x)
  else                u64.sgn(x)
