-- Problem was that this loop was mistakenly turned into a stream.
def main =
  let go _ = loop _ = [] for _ in [0, 1] do [0i32]
  in (go 0, go 1)
