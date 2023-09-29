def main [l][m][n][o] (xsss: [l][m][n][o]i64) (is: [n]i64) (is2: [m]i64)=
    #[unsafe]
    map (\xss ->
        loop res=xss[0] for i < m do
            map2 (\rs xs ->
                let wew = loop r=[xs[0]+rs[0]] for j < n do
                    r ++ [xs[i*j]+rs[i+j]]
                let wew = wew :>[o]i64
                in wew
            ) res xss[is2[i]]
    ) xsss

-- === Expected output of analysis:
