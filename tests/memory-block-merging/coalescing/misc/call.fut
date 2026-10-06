-- Short-circuiting must not move an array into another memory block
-- when it is also passed to a function call, as the call is not
-- rewritten to refer to the new memory.
-- ==
-- input { [1f32, 2f32, 3f32] }
-- output { [3f32, 5f32, 7f32, 2f32, 4f32, 6f32] }

#[noinline]
def f [n] (x: [n]f32) : [n]f32 = map (+ 1f32) x

entry main [n] (x: [n]f32) =
  let a = map (* 2f32) x
  let b = f a
  in b ++ a
