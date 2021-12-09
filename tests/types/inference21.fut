-- Zip-inference.  Note that unzip-inference is not supported (and
-- does not make much sense).

def f xs ys = zip xs ys

def main (xs: []i32) (ys: []i32) = unzip (f xs ys)
