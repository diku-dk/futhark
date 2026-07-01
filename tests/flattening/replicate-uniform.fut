-- Now we are replicating a regular array.
-- ==
-- input { [0, 1] [[4,5],[5,6]] }
-- output { [[4,5],[5,6]] }

def main = map2 (\ (i:i32) (x:[2]i32) -> let A = opaque (replicate 3 x)
                                          in #[unsafe] A[i])
