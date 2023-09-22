def main [l][n][m] (xsss: [l][n][m]i64) : i64 =
    let k = 0i64
    in #[unsafe] xsss[k,1,2]

-- === Expected output of analysis:
-- 
