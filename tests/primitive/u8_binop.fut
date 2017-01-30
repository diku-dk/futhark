-- u8 test.
--
-- ==
-- input { 0 0u8 0u8 } output { 0u8 }
-- input { 0 2u8 2u8 } output { 4u8 }
-- input { 0 127u8 127u8 } output { 254u8 }
-- input { 0 255u8 1u8 } output { 0u8 }
--
-- input { 1 2u8 2u8 } output { 0u8 }
-- input { 1 0u8 127u8 } output { 129u8 }
-- input { 1 127u8 254u8 } output { 129u8 }
--
-- input { 2 2u8 3u8 } output { 6u8 }
-- input { 2 2u8 0u8 } output { 0u8 }
-- input { 2 4u8 64u8 } output { 0u8 }
-- input { 2 5u8 64u8 } output { 64u8 }
--
-- input { 3 2u8 3u8 } output { 8u8 }
-- input { 3 2u8 0u8 } output { 1u8 }
-- input { 3 3u8 1u8 } output { 3u8 }
-- input { 3 3u8 2u8 } output { 9u8 }
-- input { 3 3u8 3u8 } output { 27u8 }
-- input { 3 3u8 4u8 } output { 81u8 }
-- input { 3 3u8 5u8 } output { 243u8 }
-- input { 3 3u8 6u8 } output { 217u8 }
-- input { 3 3u8 7u8 } output { 139u8 }

fun main(f: i32, x: u8, y: u8): u8 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
