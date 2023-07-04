-- ==
-- error: Consuming.*"xs"

def consume (xs: *[]i32) = xs

def main (xss: [][]i32) = map (\xs -> consume xs) xss
