-- Uses of a polymorphic value are separate values, so each can be consumed,
-- in the components whose type mentions the type parameter.
-- ==
-- input { 9 } output { [9] empty([0]i32) empty([0]f32) }

def empty 'a : []a = []

def pv 'a : ([]i32, []a) = ([1, 2, 3], [])

def f 't (xs: *[]t) : *[]t = xs

entry main (x: i32) =
  let a = empty : []i32
  let b = empty : []i32
  let (_, c) = pv : ([]i32, []f32)
  in (f a ++ [x], f b, f c)
