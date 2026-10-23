-- Test parser-related quirks.

type Thing = #this [2]f32 | #that [3]f32

type pt [n] 't = [n]t

type foo = pt [2] i32
type bar = pt [2] i32

type pt2 't = t
type baz = pt2 ([2]i32)
