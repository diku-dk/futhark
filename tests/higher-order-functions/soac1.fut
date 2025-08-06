-- We can use function composition in the functional argument of the reduce SOAC.
-- ==
-- input { [8,7,12,9] } output { 36 }

def compose '^a '^b '^c (f: b -> c) (g: a -> b) (x: a) : c = f (g x)

def add (x: i32) (y: i32) : i32 = x + y
def id '^a (x: a) : a = x

def main (xs: []i32) = reduce (compose add id) 0 xs
