-- ==
-- random input { [10][100]i32 } auto output

def main xss = 
    #[sequential] map (\xs -> scan (+) 0i32 xs) xss
