def main [h] [w] (n: i64) (image: *[h][w]i32) : [h][w]i32 =
  image with [:, :w - 1] = (copy image)[:, 1:]
