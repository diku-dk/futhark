def main [n][m] (xss: [n][m]i64) : [m]i64 =
    #[unsafe]
    loop res=replicate m 0 for i < n
    do
        map2 (\x r -> x+r) xss[i] res

-- === Expected output of analysis:
-- TBD