-- We can use function composition in the functional argument of the map SOAC.
-- ==
-- input { [2,4,5,1,7,5] } output { [5,9,11,3,15,11 ] }

def compose 'a 'b 'c (f: b -> c) (g: a -> b) (x: a) : c = f (g x)

def add1 (x: i32) : i32 = x + 1
def double (x: i32) : i32 = x + x

def main (xs: []i32) = map (compose add1 double) xs
