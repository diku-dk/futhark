def outerprod f x y = map (f >-> flip map y) x
def bidd A = outerprod (==) (indices A) (indices A)
def xmat A = bidd A || reverse (bidd A)
def check_matrix (A : [][]i32) = xmat A == (A != 0) |> flatten |> and
