-- Reuse memory created in kernel.  The positive variant of
-- different-base-type-sizes.fut, which has one more allocation.
--
-- FIXME: This program is not actually correct.  It can give different values
-- depending on the value of 'chunk'.  Most of this program is just cruft placed
-- in ways that result in a final program with the attributes that we want: A
-- program with a kernel with an array as thread input.  As such, we only test
-- for the number of allocs.
-- ==
-- structure gpu { Alloc 16 }

let main (n: i32, o: i32): [o]f32 =
  let membership = map (% o) (iota n)
  let values = map (\i -> f32 (2 ^ i)) (iota n) in

  let f
    (acc: *[#o]f32)
    (elem: *[#o]f32): *[o]f32 =
    map (+) acc elem

  let g
    (inp: [#chunk](f32, i32)): *[o]f32 =
    let acc = loop acc = replicate o 0.0f32 for i < chunk do
      let (values, c) = inp[i] in
      unsafe let acc[c] = acc[c] + values
      in acc
    -- Culprit, but needed to avoid fusion.
    let k0 = acc[0]
    -- acc1 can reuse the memory of acc.
    let acc1 = map (+ k0) acc
    in acc1

  in stream_red_per f g (zip values membership)
