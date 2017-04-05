-- Negative test for loops.
-- ==
-- input { [0, 3, 5, 7, 9, 11] }
-- output { [0i32, 384i32, 640i32, 896i32, 1152i32, 1408i32, 14i32, 18i32, 22i32] }

-- The number of successful coalescing should be 1!
-- The successful coalescing refers to the last
-- statement of the loop `double_mem_buffer = copy(e)`
-- which is invisible and is introduced by the compiler.
-- The rest is negativve testing: 
-- The init parameter `y` is not lastly used in the
-- initialization of the loop variant `a`. Hence the
-- coalescing of `a` (and `y`) into the memory block
-- of `w` fails. Also `y'[1]` cannot reuse the space
-- of `w` for obvious reasons (only reshape operations can).
let main(x: [#n]i32): []i32 =
  let y = map (*2) x in
  let y'= reshape (2,n/2) y
  loop(a=y) = for i < n do
      let b = map (*2) a
      let c = map (+ (b[0])) b
      let d = map (+ (c[0])) c
      let e = map (+ (d[0])) d
      in  e
  in
  let w = concat a (y'[1])
  in w

