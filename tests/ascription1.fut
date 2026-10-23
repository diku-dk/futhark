-- Basic expression-level type ascription error.
--
-- ==
-- error: f32.*i32

def main (x: i32) = x : f32
