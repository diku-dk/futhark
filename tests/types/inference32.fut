-- Inferring a unique type is never allowed - it must always be put
-- there explicitly!
-- ==
-- error: passed non-unique argument

def consume (xs: *[]i32) = xs

def main (xs: []i32) = (\xs -> consume xs) xs
