-- Horizontal fusion of reductions.
-- ==
-- structure { Screma 1 }

def main (xs: []i32) = (i32.sum xs, f32.sum (map f32.i32 xs))
