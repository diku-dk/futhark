-- Field projection inference for a lambda.
-- ==
-- input { 1 } output { [1] }

def main (x: i32) = map (\r -> r.l) [{l = x}]
