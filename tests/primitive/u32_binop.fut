-- u32 test.
--
-- ==
-- input { 0 0u32 0u32 } output { 0u32 }
-- input { 0 2u32 2u32 } output { 4u32 }
-- input { 0 2147483647u32 2147483647u32 } output { 4294967294u32 }
-- input { 0 4294967295u32 1u32 } output { 0u32 }
--
-- input { 1 2u32 2u32 } output { 0u32 }
-- input { 1 0u32 127u32 } output { 4294967169u32 }
-- input { 1 2147483647u32 4294967295u32 } output { 2147483648u32 }
--
-- input { 2 2u32 3u32 } output { 6u32 }
-- input { 2 2u32 0u32 } output { 0u32 }
-- input { 2 262144u32 262144u32 } output { 0u32 }
-- input { 2 262145u32 262144u32 } output { 262144u32 }
--
-- input { 3 2u32 3u32 } output { 8u32 }
-- input { 3 47u32 0u32 } output { 1u32 }
-- input { 3 47u32 1u32 } output { 47u32 }
-- input { 3 47u32 2u32 } output { 2209u32 }
-- input { 3 47u32 3u32 } output { 103823u32 }
-- input { 3 47u32 4u32 } output { 4879681u32 }
-- input { 3 47u32 5u32 } output { 229345007u32 }
-- input { 3 47u32 6u32 } output { 2189280737u32 }
-- input { 3 47u32 7u32 } output { 4111946831u32 }

fun main(f: int, x: u32, y: u32): u32 =
  if      f == 0 then x + y
  else if f == 1 then x - y
  else if f == 2 then x * y
  else           x ** y
