-- u16 test.
--
-- ==
-- input { 0 0u16 0u16 } output { 0u16 }
-- input { 0 2u16 2u16 } output { 4u16 }
-- input { 0 32767u16 32767u16 } output { 65534u16 }
-- input { 0 65535u16 1u16 } output { 0u16 }
--
-- input { 1 2u16 2u16 } output { 0u16 }
-- input { 1 0u16 127u16 } output { 65409u16 }
-- input { 1 32767u16 65534u16 } output { 32769u16 }
--
-- input { 2 2u16 3u16 } output { 6u16 }
-- input { 2 2u16 0u16 } output { 0u16 }
-- input { 2 256u16 256u16 } output { 0u16 }
-- input { 2 257u16 256u16 } output { 256u16 }
--
-- input { 3 2u16 3u16 } output { 8u16 }
-- input { 3 7u16 0u16 } output { 1u16 }
-- input { 3 7u16 1u16 } output { 7u16 }
-- input { 3 7u16 2u16 } output { 49u16 }
-- input { 3 7u16 3u16 } output { 343u16 }
-- input { 3 7u16 4u16 } output { 2401u16 }
-- input { 3 7u16 5u16 } output { 16807u16 }
-- input { 3 7u16 6u16 } output { 52113u16 }
-- input { 3 7u16 7u16 } output { 37111u16 }

let main(f: i32, x: u16, y: u16): u16 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
