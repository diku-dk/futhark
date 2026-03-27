-- ==
-- tags { no_webgpu }

def main (xs: []f64) = reduce f64.max (-f64.inf) xs
