-- Tests various complicated sum types and destructing/constructing
-- them.

type~ contrived = #foo ([]i32) bool | #bar bool ([]u32) | #baz ([]i32) ([]i32)

entry next (c: contrived) : contrived =
  match c
  case #foo arr b -> #bar (!b) (map u32.i32 (map (+1) arr))
  case #bar b arr -> #baz (map i32.u32 (map (*2) arr)) (map i32.u32 (map (+u32.bool b) arr))
  case #baz x y -> #foo (map2 (+) x y) (i32.sum x % 2 == 0)
