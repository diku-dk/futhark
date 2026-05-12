-- Futhark supports hexadecimal float literals
-- ==
-- input {}
-- output {
-- [31.875f64, 31.875f64, 17.996094f64, 3.984375f64, -17.996094f64]
-- [31.875f32, 17.996094f32, 3.984375f32, -17.996094f32, 0.9375f32]
-- }

def main : ([]f64, []f32) =
  ( [0xf.fp1, 0xf.fp1f64, 0x11.ffp0f64, 0xf.fp-2f64, -0x11.ffp0_0f64]
  , [0xf.fp1f32, 0x11.ffp0f32, 0xf.fp-2f32, -0x11.ffp0f32, 0x0.f0p0f32]
  )
