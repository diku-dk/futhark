-- ==
-- script input { (2f32, $loaddata "data/input.in") }
-- output { [3f32,4f32,5f32] }
-- "the other one" script input { (3f32, $loaddata "data/input.in") }
-- output { [4f32,5f32,6f32] }

def main (x: f32) arr = map (+ x) arr
