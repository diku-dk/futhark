def outerprod f x y = map (f >-> flip map y) x
def bidd A = outerprod (==) (indices A) (indices A)
def xmat A = map2 (map2 (||)) (bidd A) (map reverse (bidd A))
def check_matrix A =
  map2 (map2 (==)) (xmat A) (map (map (!=0i32)) A)
  |> map and |> and

-- Tests
def a = check_matrix [[2, 0, 0, 1],
                      [0, 3, 1, 0],
                      [0, 5, 2, 0],
                      [4, 0, 0, 2]]
def b = check_matrix [[5, 7, 0],
                      [0, 3, 1],
                      [0, 5, 0]]
