entry main [m][n] (xss: [m][n]i64) =
    map(\xs ->
    foldl (+) 0 xs
    + foldl (*) 2 xs
    + foldl (-) 0 xs
    ) xss
