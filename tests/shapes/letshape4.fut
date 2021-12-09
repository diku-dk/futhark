-- The monomorphiser forgot to keep around the 'n' in this program at
-- one point.

def n = 1i64
def vec 't arr = arr : [n]t
def main (xs: []i32) = vec xs
