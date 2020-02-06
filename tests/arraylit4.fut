-- Got screwed up at one point, because the types may be more complex
-- in the core language.
-- ==

let main (k2p2: i32) (N: i32) : [k2p2][N]f32 =
  [map r32 (iota N)] :> [k2p2][N]f32
