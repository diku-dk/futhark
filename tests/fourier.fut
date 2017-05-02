-- A slow O(n**2) Fourier transform (SFT).
--
-- Based on EWD807, but with a correction.  When computing 'x',
-- Dijkstra specifies a constant '2' in the exponents, while most
-- formulations (like Wikipedia and Numpy) use -2.  I have gone with
-- the latter.  This affects the sign of the imaginary part of the
-- result.
--
-- ==
-- input { [0f32,1f32,2f32,3f32,4f32,5f32,6f32,7f32,8f32,9f32 ] }
-- output {
--   [45f32, -5f32, -5f32, -5f32,
--    -5f32, -5f32, -5f32, -5f32,
--    -5f32, -5f32]
--   [0.0f32, 15.388417f32, 6.881909f32, 3.632712f32,
--    1.624598f32, -5.510910e-15f32, -1.624598f32,
--    -3.632712f32, -6.881909f32, -15.388417f32]
-- }

import "/futlib/math"

default (f32)

let pi: f32 = f32.acos 0.0 * 2.0

type complex = (f32, f32)

let complexAdd ((a, b): complex) ((c, d): complex): complex =
  (a+c, b+d)

let complexMult ((a,b): complex) ((c,d): complex): complex =
  (a*c - b*d,
   a*d + b*c)

let toComplex (a: f32): complex = (a, 0f32)

let complexExp ((a,b): complex): complex =
  complexMult (toComplex (f32.exp a)) (f32.cos b, f32.sin b)

let toPolar ((a,b): complex): (f32, f32) =
  (f32.sqrt (a*a + b*b),
   f32.atan (b/a))

let fromPolar (r: f32, angle: f32): complex =
  (r * f32.cos angle,
   r * f32.sin angle)

let complexPow (c: complex) (n: i32): complex =
  let (r, angle) = toPolar c
  let (r', angle') = (r ** f32 n,
                      f32 n * angle)
  in fromPolar (r', angle')

let f(a: [#n]f32) (j: i32): complex =
  let x = complexExp (complexMult (-2.0,0.0)
                      (complexMult (toComplex pi)
                       (complexMult (0.0, 1.0)
                        (toComplex (1.0/f32 n)))))
  in reduce complexAdd (0.0, 0.0)
  (map complexMult
   (map toComplex a)
   (map (complexPow x) (map (j*) (iota n))))

let sft(a: [#n]f32): [n]complex =
  map (f a) (iota n)

let main(a: [#n]f32): ([n]f32, [n]f32) = unzip (sft a)
