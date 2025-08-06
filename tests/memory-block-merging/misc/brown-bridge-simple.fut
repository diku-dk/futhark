-- Test inspired from Option Pricing's brownian bridge
-- ==
-- input { [ [1.0, 3.0, 5.0, 7.0, 9.0, 11.0], [0.0, 2.0, 4.0, 6.0, 8.0, 10.0] ] }
-- output { [[3.0f64, 2.0f64, 2.0f64, 2.0f64, 2.75f64, -11.25f64], [2.0f64, 2.0f64, 2.0f64, 2.0f64, 2.0f64, -10.0f64]] }

def brownian_bridge [num_dates] (gauss: [num_dates]f64) : [num_dates]f64 =
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

def main [m] [num_dates] (gausses: [m][num_dates]f64) : [m][num_dates]f64 =
  map brownian_bridge gausses
