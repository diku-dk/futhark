-- u8 test.

entry add = map2 (u8.+)
entry sub = map2 (u8.-)
entry mul = map2 (u8.*)
entry pow = map2 (u8.**)

-- ==
-- entry: add
-- input  { [0u8, 2u8, 127u8, 255u8]
--          [0u8, 2u8, 127u8, 1u8] }
-- output { [0u8, 4u8, 254u8, 0u8] }

-- ==
-- entry: sub
-- input  { [2u8, 0u8, 127u8]
--          [2u8, 127u8, 254u8] }
-- output { [0u8, 129u8, 129u8] }

-- ==
-- entry: mul
-- input  { [2u8, 2u8,  4u8,  5u8]
--          [3u8, 0u8, 64u8, 64u8] }
-- output { [6u8, 0u8, 0u8, 64u8] }

-- ==
-- entry: pow
-- input  { [2u8, 2u8, 3u8, 3u8, 3u8, 3u8, 3u8, 3u8, 3u8]
--          [3u8, 0u8, 1u8, 2u8, 3u8, 4u8, 5u8, 6u8, 7u8] }
-- output { [8u8, 1u8, 3u8, 9u8, 27u8, 81u8, 243u8, 217u8, 139u8] }
