-- Reuse not possible because of different base type sizes, so the arrays take
-- up different amounts of memory.  See also reuse-in-kernel.fut.
-- ==
-- structure gpu { Alloc 17 }

let main (n: i32, o: i32): [o]f32 =
  let membership = map (% o) (iota n)
  let values = map (\i -> f32.i32 (2 ^ i)) (iota n) in

  let f [o]
    (acc: *[o]f32)
    (elem: *[o]f32): *[o]f32 =
    map (+) acc elem

  let g [chunk]
    (inp: [chunk](f32, i32)): *[o]f32 =
    let acc = loop acc = replicate o 0.0f32 for i < chunk do
      let (values, c) = inp[i] in
      unsafe let acc[c] = acc[c] + values
      in acc
    -- Culprits, but needed to avoid fusion.
    let k0 = acc[0]
    -- acc1 cannot reuse the memory of acc, since its base type uses 8 bytes
    -- instead of acc's 4 bytes.
    let acc1 = map (\t -> i64.f32 (t + k0)) acc
    let k1 = acc1[0]
    -- acc2 can reuse memory (but that's not the point).
    let acc2 = map (\t -> f32.i64 (t + k1)) acc1
    in acc2

  in stream_red_per f g (zip values membership)
