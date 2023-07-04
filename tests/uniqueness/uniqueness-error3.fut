-- ==
-- error: "a".*consumed

def main n =
    let a = iota(n)
    let b = a -- b and a alias each other.
    let (i,j) = (2,5) in
    (let a[i]=b[j] in a[i]) -- Consume a, and thus also b.
    + b[j] -- Error!
