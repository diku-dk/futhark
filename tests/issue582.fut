-- Turned out to be a bug in the fuse-across-transpose optimisation.
-- ==
-- input { [[[1f32], [2f32]], [[3f32], [4f32]]] }
-- output { [[[4.0f32, 8.0f32]], [[12.0f32, 16.0f32]]] }

type two 't = [2]t

def main (a: [][][]f32) : [][][]f32 =
  let a = map (map (map (* 2))) a
  let a = map (map (map (* 2))) <| transpose <| map transpose a
  in transpose a
