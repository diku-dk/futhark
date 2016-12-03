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

default (f32)

val pi: f32 = acos32 0.0 * 2.0

type complex = (f32, f32)

fun complexAdd ((a, b): complex) ((c, d): complex): complex =
  (a+c, b+d)

fun complexMult ((a,b): complex) ((c,d): complex): complex =
  (a*c - b*d,
   a*d + b*c)

fun toComplex (a: f32): complex = (a, 0f32)

fun complexExp ((a,b): complex): complex =
  complexMult (toComplex (exp32 a)) (cos32 b, sin32 b)

fun toPolar ((a,b): complex): (f32, f32) =
  (sqrt32 (a*a + b*b),
   atan32 (b/a))

fun fromPolar (r: f32, angle: f32): complex =
  (r * cos32 angle,
   r * sin32 angle)

fun complexPow (c: complex) (n: int): complex =
  let (r, angle) = toPolar c
  let (r', angle') = (r ** f32 n,
                      f32 n * angle)
  in fromPolar (r', angle')

fun f(a: [n]f32) (j: int): complex =
  let x = complexExp (complexMult (-2.0,0.0)
                      (complexMult (toComplex pi)
                       (complexMult (0.0, 1.0)
                        (toComplex (1.0/f32 n)))))
  in reduce complexAdd (0.0, 0.0)
  (map complexMult
   (map toComplex a)
   (map (complexPow x) (map (j*) (iota n))))

fun sft(a: [n]f32): [n]complex =
  map (f a) (iota n)

fun main(a: [n]f32): ([n]f32, [n]f32) = unzip (sft a)
