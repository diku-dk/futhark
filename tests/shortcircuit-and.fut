-- && must be short-circuiting.
--
-- ==
-- tags { no_webgpu }
-- input { 0i64 [true, true] } output { true }
-- input { 1i64 [true, true] } output { true }
-- input { 2i64 [true, true] } output { false }

def main [n] (i: i64) (bs: [n]bool) : bool =
  i < n && bs[i]
