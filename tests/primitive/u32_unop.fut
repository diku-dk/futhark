-- Test unary operators for u32.
--
-- ==
-- input { 0  0u32 } output {  0u32 }
-- input { 0  1u32 } output { 4294967295u32 }
-- input { 0 4294967295u32 } output {  1u32 }
-- input { 0  8u32 } output { 4294967288u32 }
-- input { 0 4294967288u32 } output {  8u32 }
--
-- input { 1  0u32 } output { 0u32 }
-- input { 1  1u32 } output { 1u32 }
-- input { 1 4294967295u32 } output { 4294967295u32 }
-- input { 1  8u32 } output { 8u32 }
-- input { 1 4294967288u32 } output { 4294967288u32 }
--
-- input { 2  0u32 } output { 0u32 }
-- input { 2  1u32 } output { 1u32 }
-- input { 2 4294967295u32 } output { 1u32 }
-- input { 2  8u32 } output { 1u32 }
-- input { 2 4294967288u32 } output { 1u32 }


let main (f: i32) (x: u32): u32 =
  if      f == 0 then -x
  else if f == 1 then u32.abs(x)
  else                u32.sgn(x)
