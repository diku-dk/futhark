def main [l][n][m] (xsss: [l][n][m]i64) : [l]i64 =
    map2 (\xss i -> #[unsafe] xss[0,i]) xsss (iota l)

-- === Expected output of analysis:
-- TBD