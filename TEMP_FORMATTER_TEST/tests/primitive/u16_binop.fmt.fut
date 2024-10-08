-- u16 test.
entry add = map2 u16.(+)

entry sub = map2 u16.(-)

entry mul = map2 u16.(*)

entry pow = map2 u16.(**)-- ==
-- entry: add
-- input  { [0u16, 2u16, 32767u16, 65535u16]
--          [0u16, 2u16, 32767u16, 1u16] }
-- output { [0u16, 4u16, 65534u16, 0u16] }
-- ==
-- entry: sub
-- input  { [2u16, 0u16, 32767u16]
--          [2u16, 127u16, 65534u16] }
-- output { [0u16, 65409u16, 32769u16] }
-- ==
-- entry: mul
-- input  { [2u16, 2u16,  256u16,  257u16]
--          [3u16, 0u16, 256u16, 256u16] }
-- output { [6u16, 0u16, 0u16, 256u16] }
-- ==
-- entry: pow
-- input  { [2u16, 7u16, 7u16, 7u16, 7u16, 7u16, 7u16, 7u16, 7u16]
--          [3u16, 0u16, 1u16, 2u16, 3u16, 4u16, 5u16, 6u16, 7u16] }
-- output { [8u16, 1u16, 7u16, 49u16, 343u16, 2401u16, 16807u16, 52113u16, 37111u16] }
