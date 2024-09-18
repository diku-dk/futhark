def moving_point (x: i64, y: i64) : ([][]u32, (i64, i64)) =
  let canvas = replicate 128 (replicate 128 0) with [y, x] = 0x00FFFFFF
  in (canvas, (x + 1, y+1))

-- > :video (moving_point, (10i64, 10i64), 50i64,);
-- fps: 1
-- format: gif
