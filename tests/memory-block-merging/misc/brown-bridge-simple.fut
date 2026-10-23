-- Test inspired from Option Pricing's brownian bridge
-- ==
-- input { [ [1.0f32, 3.0f32, 5.0f32, 7.0f32, 9.0f32, 11.0f32], [0.0f32, 2.0f32, 4.0f32, 6.0f32, 8.0f32, 10.0f32] ] }
-- output { [[3.0f32, 2.0f32, 2.0f32, 2.0f32, 2.75f32, -11.25f32], [2.0f32, 2.0f32, 2.0f32, 2.0f32, 2.0f32, -10.0f32]] }

def brownian_bridge [num_dates] (gauss: [num_dates]f32) : [num_dates]f32 =
  let bbrow = replicate num_dates 0.0
  let bbrow[num_dates - 1] = 0.5 * gauss[0]
  let bbrow =
    loop (bbrow)
    for i < num_dates - 1 do
      #[unsafe]
      let bbrow[i] = bbrow[i + 1] * 1.5 + gauss[i + 1]
      in bbrow
  let bbrow =
    loop (bbrow)
    for ii < num_dates - 1 do
      let i = num_dates - (ii + 1)
      let bbrow[i] = bbrow[i] - bbrow[i - 1]
      in bbrow
  in bbrow

def main [m] [num_dates] (gausses: [m][num_dates]f32) : [m][num_dates]f32 =
  map brownian_bridge gausses
