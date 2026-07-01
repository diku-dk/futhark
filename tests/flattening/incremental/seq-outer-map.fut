-- ==
-- random input { [10][100]i32 } auto output

def main xss = 
    #[sequential_outer] map (\xs -> map (+100) xs) xss