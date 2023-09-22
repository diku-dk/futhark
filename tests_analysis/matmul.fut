-- Matrix multiplication
def main [n] (xss: [n][n]i32) (yss: [n][n]i32) : [n][n]i32 =
    map (\xs_row ->
        map (\ys_col ->
            -- #[sequential]
            -- reduce (+) 0 (map2 (*) xs_row ys_col)
            foldl (+) 0 (map2 (*) xs_row ys_col)
        ) (transpose yss)
    ) xss

-- === Expected output of analysis:
-- TBD
