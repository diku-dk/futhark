def (>->>) '^a '^b '^c (f: a -> b) (g: b -> c) (x: a) : c = g (f x)
def (<-<<) '^a '^b '^c (g: b -> c) (f: a -> b) (x: a) : c = g (f x)

def compose2 = ((>->) (<-<) <-<) (>->)

entry main = compose2 (+) (* 2i32)
