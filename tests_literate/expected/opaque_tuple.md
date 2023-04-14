Based on #1750.

```futhark
type vec2 = (i64, i64)

def moving_point (x: i64, y: i64) : ([][]u32, vec2) =
  let canvas = replicate 128 (replicate 128 0) with [y, x] = 255
  in (canvas, (x + 1, y+1))
```

```
> :video (moving_point, (10i64, 10i64), 50i64);
fps: 1
format: gif
```

![](opaque_tuple-img/da140d8304219232f384b6a2d5ea4be9-video.gif)
