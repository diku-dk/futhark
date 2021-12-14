-- Got screwed up at one point, because the types may be more complex
-- in the core language.
-- ==

def main (k2p2: i64) (N: i64) : [k2p2][N]f32 =
  [map f32.i64 (iota N)] :> [k2p2][N]f32
