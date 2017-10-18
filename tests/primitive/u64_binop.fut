-- u64 test.
--
-- ==
-- input { 0 0u64 0u64 } output { 0u64 }
-- input { 0 2u64 2u64 } output { 4u64 }
-- input { 0 9223372036854775807u64 9223372036854775807u64 } output { 18446744073709551614u64 }
-- input { 0 18446744073709551615u64 1u64 } output { 0u64 }
--
-- input { 1 2u64 2u64 } output { 0u64 }
-- input { 1 0u64 127u64 } output { 18446744073709551489u64 }
-- input { 1 9223372036854775808u64 18446744073709551615u64 } output { 9223372036854775809u64 }
--
-- input { 2 2u64 3u64 } output { 6u64 }
-- input { 2 2u64 0u64 } output { 0u64 }
-- input { 2 6442450941u64 2147483647u64 } output { 13835058042397261827u64 }
--
-- input { 3 2u64 3u64 } output { 8u64 }
-- input { 3 4021u64 0u64 } output { 1u64 }
-- input { 3 4021u64 1u64 } output { 4021u64 }
-- input { 3 4021u64 2u64 } output { 16168441u64 }
-- input { 3 4021u64 3u64 } output { 65013301261u64 }
-- input { 3 4021u64 4u64 } output { 261418484370481u64 }
-- input { 3 4021u64 5u64 } output { 1051163725653704101u64 }
-- input { 3 4021u64 6u64 } output { 2424947974056870057u64 }
-- input { 3 4021u64 7u64 } output { 10834932764031245949u64 }

let main(f: i32, x: u64, y: u64): u64 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
