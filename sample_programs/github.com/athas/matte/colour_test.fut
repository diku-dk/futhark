-- | ignore

-- Proper tests of this library require drawing colours to the screen,
-- I think.

import "colour"

-- ==
-- entry: basic_mix
-- input {} output {0.7058824f32 0.7058824f32 0.7058824f32 1.0f32}
entry basic_mix =
  argb.to_rgba (argb.mix 0.5f32 argb.white 0.5f32 argb.black)

-- ==
-- entry: is_argb
-- input {} output {0xFF000000u32}
entry is_argb: u32 = argb.black
